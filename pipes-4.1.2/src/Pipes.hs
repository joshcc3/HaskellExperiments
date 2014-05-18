{-# LANGUAGE
    RankNTypes
  , FlexibleInstances
  , MultiParamTypeClasses
  , UndecidableInstances
  , Trustworthy
  #-}

{-| This module is the recommended entry point to the @pipes@ library.

    Read "Pipes.Tutorial" if you want a tutorial explaining how to use this
    library.
-}

module Pipes (
    -- * The Proxy Monad Transformer
      Proxy
    , X
    , Effect
    , Effect'
    , runEffect

    -- ** Producers
    -- $producers
    , Producer
    , Producer'
    , yield
    , for
    , (~>)
    , (<~)

    -- ** Consumers
    -- $consumers
    , Consumer
    , Consumer'
    , await
    , (>~)
    , (~<)

    -- ** Pipes
    -- $pipes
    , Pipe
    , cat
    , (>->)
    , (<-<)

    -- * ListT
    , ListT(..)
    , runListT
    , Enumerable(..)

    -- * Utilities
    , next
    , each
    , every
    , discard

    -- * Re-exports
    -- $reexports
    , module Control.Monad.IO.Class
    , module Control.Monad.Trans.Class
    , module Control.Monad.Morph
    , module Data.Foldable
    ) where

import Control.Applicative (Applicative(pure, (<*>)), Alternative(empty, (<|>)))
import Control.Monad.Error (MonadError(..))
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad (MonadPlus(mzero, mplus))
import Control.Monad.Reader (MonadReader(..))
import Control.Monad.State (MonadState(..))
import Control.Monad.Trans.Class (MonadTrans(lift))
import Control.Monad.Trans.Error (ErrorT(runErrorT))
import Control.Monad.Trans.Identity (IdentityT(runIdentityT))
import Control.Monad.Trans.Maybe (MaybeT(runMaybeT))
import Control.Monad.Writer (MonadWriter(..))
import Data.Foldable (Foldable)
import Data.Monoid (Monoid(..))
import Pipes.Core
import Pipes.Internal (Proxy(..))
import qualified Data.Foldable as F

-- Re-exports
import Control.Monad.Morph (MFunctor(hoist))

infixl 4 <~
infixr 4 ~>
infixl 5 ~<
infixr 5 >~
infixl 7 >->
infixr 7 <-<

{- $producers
    Use 'yield' to produce output and ('~>') \/ 'for' to substitute 'yield's.

    'yield' and ('~>') obey the 'Control.Category.Category' laws:

@
\-\- Substituting \'yield\' with \'f\' gives \'f\'
'yield' '~>' f = f

\-\- Substituting every \'yield\' with another \'yield\' does nothing
f '~>' 'yield' = f

\-\- \'yield\' substitution is associative
(f '~>' g) '~>' h = f '~>' (g '~>' h)
@

    These are equivalent to the following \"for loop laws\":

@
\-\- Looping over a single yield simplifies to function application
'for' ('yield' x) f = f x

\-\- Re-yielding every element of a stream returns the original stream
'for' s 'yield' = s

\-\- Nested for loops can become a sequential 'for' loops if the inner loop
\-\- body ignores the outer loop variable
'for' s (\\a -\> 'for' (f a) g) = 'for' ('for' s f) g = 'for' s (f '~>' g)
@

-}

{-| Produce a value

@
'yield' :: 'Monad' m => a -> 'Pipe' x a m ()
@
-}
yield :: Monad m => a -> Producer' a m ()
yield = respond
{-# INLINABLE yield #-}

{-| @(for p body)@ loops over @p@ replacing each 'yield' with @body@.

@
'for' :: 'Monad' m => 'Producer' b m r -> (b -> 'Effect'       m ()) -> 'Effect'       m r
'for' :: 'Monad' m => 'Producer' b m r -> (b -> 'Producer'   c m ()) -> 'Producer'   c m r
'for' :: 'Monad' m => 'Pipe'   x b m r -> (b -> 'Consumer' x   m ()) -> 'Consumer' x   m r
'for' :: 'Monad' m => 'Pipe'   x b m r -> (b -> 'Pipe'     x c m ()) -> 'Pipe'     x c m r
@
-}
for :: Monad m
    =>       Proxy x' x b' b m a'
    -- ^
    -> (b -> Proxy x' x c' c m b')
    -- ^
    ->       Proxy x' x c' c m a'
for = (//>)
{-# INLINABLE for #-}

{-# RULES
    "for (for p f) g" forall p f g . for (for p f) g = for p (\a -> for (f a) g)

  ; "for p yield" forall p . for p yield = p

  ; "for (yield x) f" forall x f . for (yield x) f = f x

  ; "for cat f" forall f .
        for cat f =
            let go = do
                    x <- await
                    f x
                    go
            in  go

  ; "f >~ (g >~ p)" forall f g p . f >~ (g >~ p) = (f >~ g) >~ p

  ; "await >~ p" forall p . await >~ p = p

  ; "p >~ await" forall p . p >~ await = p

  ; "m >~ cat" forall m .
        m >~ cat =
            let go = do
                    x <- m
                    yield x
                    go
            in  go

  ; "p1 >-> (p2 >-> p3)" forall p1 p2 p3 .
        p1 >-> (p2 >-> p3) = (p1 >-> p2) >-> p3

  ; "p >-> cat" forall p . p >-> cat = p

  ; "cat >-> p" forall p . cat >-> p = p

  #-}

{-| Compose loop bodies

@
('~>') :: 'Monad' m => (a -> 'Producer' b m r) -> (b -> 'Effect'       m ()) -> (a -> 'Effect'       m r)
('~>') :: 'Monad' m => (a -> 'Producer' b m r) -> (b -> 'Producer'   c m ()) -> (a -> 'Producer'   c m r)
('~>') :: 'Monad' m => (a -> 'Pipe'   x b m r) -> (b -> 'Consumer' x   m ()) -> (a -> 'Consumer' x   m r)
('~>') :: 'Monad' m => (a -> 'Pipe'   x b m r) -> (b -> 'Pipe'     x c m ()) -> (a -> 'Pipe'     x c m r)
@
-}
(~>)
    :: Monad m
    => (a -> Proxy x' x b' b m a')
    -- ^
    -> (b -> Proxy x' x c' c m b')
    -- ^
    -> (a -> Proxy x' x c' c m a')
(~>) = (/>/)
{-# INLINABLE (~>) #-}

-- | ('~>') with the arguments flipped
(<~)
    :: Monad m
    => (b -> Proxy x' x c' c m b')
    -- ^
    -> (a -> Proxy x' x b' b m a')
    -- ^
    -> (a -> Proxy x' x c' c m a')
g <~ f = f ~> g
{-# INLINABLE (<~) #-}

{- $consumers
    Use 'await' to request input and ('>~') to substitute 'await's.

    'await' and ('>~') obey the 'Control.Category.Category' laws:

@
\-\- Substituting every \'await\' with another \'await\' does nothing
'await' '>~' f = f

\-\- Substituting \'await\' with \'f\' gives \'f\'
f '>~' 'await' = f

\-\- \'await\' substitution is associative
(f '>~' g) '>~' h = f '>~' (g '>~' h)
@

-}

{-| Consume a value

@
'await' :: 'Monad' m => 'Pipe' a y m a
@
-}
await :: Monad m => Consumer' a m a
await = request ()
{-# INLINABLE await #-}

{-| @(draw >~ p)@ loops over @p@ replacing each 'await' with @draw@

@
('>~') :: 'Monad' m => 'Effect'       m b -> 'Consumer' b   m c -> 'Effect'       m c
('>~') :: 'Monad' m => 'Consumer' a   m b -> 'Consumer' b   m c -> 'Consumer' a   m c
('>~') :: 'Monad' m => 'Producer'   y m b -> 'Pipe'     b y m c -> 'Producer'   y m c
('>~') :: 'Monad' m => 'Pipe'     a y m b -> 'Pipe'     b y m c -> 'Pipe'     a y m c
@
-}
(>~)
    :: Monad m
    => Proxy a' a y' y m b
    -- ^
    -> Proxy () b y' y m c
    -- ^
    -> Proxy a' a y' y m c
p1 >~ p2 = (\() -> p1) >\\ p2
{-# INLINABLE (>~) #-}

-- | ('>~') with the arguments flipped
(~<)
    :: Monad m
    => Proxy () b y' y m c
    -- ^
    -> Proxy a' a y' y m b
    -- ^
    -> Proxy a' a y' y m c
p2 ~< p1 = p1 >~ p2
{-# INLINABLE (~<) #-}

{- $pipes
    Use 'await' and 'yield' to build 'Pipe's and ('>->') to connect 'Pipe's.

    'cat' and ('>->') obey the 'Control.Category.Category' laws:

@
\-\- Useless use of cat
'cat' '>->' f = f

\-\- Redirecting output to cat does nothing
f '>->' 'cat' = f

\-\- The pipe operator is associative
(f '>->' g) '>->' h = f '>->' (g '>->' h)
@

-}

-- | The identity 'Pipe', analogous to the Unix @cat@ program
cat :: Monad m => Pipe a a m r
cat = pull ()
{-# INLINABLE cat #-}

{-| 'Pipe' composition, analogous to the Unix pipe operator

@
('>->') :: 'Monad' m => 'Producer' b m r -> 'Consumer' b   m r -> 'Effect'       m r
('>->') :: 'Monad' m => 'Producer' b m r -> 'Pipe'     b c m r -> 'Producer'   c m r
('>->') :: 'Monad' m => 'Pipe'   a b m r -> 'Consumer' b   m r -> 'Consumer' a   m r
('>->') :: 'Monad' m => 'Pipe'   a b m r -> 'Pipe'     b c m r -> 'Pipe'     a c m r
@
-}
(>->)
    :: Monad m
    => Proxy a' a () b m r
    -- ^
    -> Proxy () b c' c m r
    -- ^
    -> Proxy a' a c' c m r
p1 >-> p2 = (\() -> p1) +>> p2
{-# INLINABLE (>->) #-}

{-| The list monad transformer, which extends a monad with non-determinism

    'return' corresponds to 'yield', yielding a single value

    ('>>=') corresponds to 'for', calling the second computation once for each
    time the first computation 'yield's.
-}
newtype ListT m a = Select { enumerate :: Producer a m () }

instance (Monad m) => Functor (ListT m) where
    fmap f p = Select (for (enumerate p) (\a -> yield (f a)))

instance (Monad m) => Applicative (ListT m) where
    pure a = Select (yield a)
    mf <*> mx = Select (
        for (enumerate mf) (\f ->
        for (enumerate mx) (\x ->
        yield (f x) ) ) )

instance (Monad m) => Monad (ListT m) where
    return a = Select (yield a)
    m >>= f  = Select (for (enumerate m) (\a -> enumerate (f a)))
    fail _   = mzero

instance MonadTrans ListT where
    lift m = Select (do
        a <- lift m
        yield a )

instance (MonadIO m) => MonadIO (ListT m) where
    liftIO m = lift (liftIO m)

instance (Monad m) => Alternative (ListT m) where
    empty = Select (return ())
    p1 <|> p2 = Select (do
        enumerate p1
        enumerate p2 )

instance (Monad m) => MonadPlus (ListT m) where
    mzero = empty
    mplus = (<|>)

instance MFunctor ListT where
    hoist morph = Select . hoist morph . enumerate

instance (Monad m) => Monoid (ListT m a) where
    mempty = empty
    mappend = (<|>)

instance (MonadState s m) => MonadState s (ListT m) where
    get     = lift  get

    put   s = lift (put   s)

    state f = lift (state f)

instance (MonadWriter w m) => MonadWriter w (ListT m) where
    writer = lift . writer

    tell w = lift (tell w)

    listen l = Select (go (enumerate l) mempty)
      where
        go p w = case p of
            Request a' fa  -> Request a' (\a  -> go (fa  a ) w)
            Respond b  fb' -> Respond (b, w)  (\b' -> go (fb' b') w)
            M          m   -> M (do
                (p', w') <- listen m
                return (go p' $! mappend w w') )
            Pure    r      -> Pure r

    pass l = Select (go (enumerate l) mempty)
      where
        go p w = case p of
            Request  a'     fa  -> Request a' (\a  -> go (fa  a ) w)
            Respond (b, f)  fb' -> M (pass (return
                (Respond b (\b' -> go (fb' b') (f w)), \_ -> f w) ))
            M               m   -> M (do
                (p', w') <- listen m
                return (go p' $! mappend w w') )
            Pure     r          -> Pure r

instance (MonadReader i m) => MonadReader i (ListT m) where
    ask = lift ask

    local f l = Select (local f (enumerate l))

    reader f = lift (reader f)

instance (MonadError e m) => MonadError e (ListT m) where
    throwError e = lift (throwError e)

    catchError l k = Select (catchError (enumerate l) (\e -> enumerate (k e)))

-- | Run a self-contained `ListT` computation
runListT :: Monad m => ListT m X -> m ()
runListT l = runEffect (enumerate l)
{-# INLINABLE runListT #-}

{-| 'Enumerable' generalizes 'Data.Foldable.Foldable', converting effectful
    containers to 'ListT's.
-}
class Enumerable t where
    toListT :: Monad m => t m a -> ListT m a

instance Enumerable ListT where
    toListT = id

instance Enumerable IdentityT where
    toListT m = Select $ do
        a <- lift $ runIdentityT m
        yield a

instance Enumerable MaybeT where
    toListT m = Select $ do
        x <- lift $ runMaybeT m
        case x of
            Nothing -> return ()
            Just a  -> yield a

instance Enumerable (ErrorT e) where
    toListT m = Select $ do
        x <- lift $ runErrorT m
        case x of
            Left  _ -> return ()
            Right a -> yield a

{-| Consume the first value from a 'Producer'

    'next' either fails with a 'Left' if the 'Producer' terminates or succeeds
    with a 'Right' providing the next value and the remainder of the 'Producer'.
-}
next :: Monad m => Producer a m r -> m (Either r (a, Producer a m r))
next = go
  where
    go p = case p of
        Request v _  -> closed v
        Respond a fu -> return (Right (a, fu ()))
        M         m  -> m >>= go
        Pure    r    -> return (Left r)
{-# INLINABLE next #-}

-- | Convert a 'F.Foldable' to a 'Producer'
each :: (Monad m, Foldable f) => f a -> Producer' a m ()
each = F.foldr (\a p -> yield a >> p) (return ())
{-# INLINABLE each #-}
{-  The above code is the same as:

> each = Data.Foldable.mapM_ yield

    ... except writing it directly in terms of `Data.Foldable.foldr` improves
    build/foldr fusion
-}

-- | Convert an 'Enumerable' to a 'Producer'
every :: (Monad m, Enumerable t) => t m a -> Producer' a m ()
every it = discard >\\ enumerate (toListT it)
{-# INLINABLE every #-}

-- | Discards a value
discard :: Monad m => a -> m ()
discard _ = return ()
{-# INLINABLE discard #-}

-- | ('>->') with the arguments flipped
(<-<)
    :: Monad m
    => Proxy () b c' c m r
    -- ^
    -> Proxy a' a () b m r
    -- ^
    -> Proxy a' a c' c m r
p2 <-< p1 = p1 >-> p2
{-# INLINABLE (<-<) #-}

{- $reexports
    "Control.Monad.IO.Class" re-exports 'MonadIO'.

    "Control.Monad.Trans.Class" re-exports 'MonadTrans'.

    "Control.Monad.Morph" re-exports 'MFunctor'.

    "Data.Foldable" re-exports 'Foldable' (the class name only)
-}
