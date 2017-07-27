-- Packages installed: split text hashtables vector
-- vector-algorithms parsec bytestring unordered-containers
-- hashable scientific random async stm aeson
-- Run using: runghc file.hs

{-# LANGUAGE TypeSynonymInstances, FlexibleContexts, IncoherentInstances, 
             MultiParamTypeClasses, TupleSections, FlexibleInstances, 
             ConstraintKinds, DeriveFunctor, GeneralizedNewtypeDeriving, 
             GADTs, RankNTypes, ImplicitParams #-}

import Control.Applicative
import Data.Functor.Identity
import Data.Monoid
import Control.Monad.State
import Data.List

class Comonad c where
  extract :: c a -> a
  duplicate :: c a -> c (c a)

instance Monoid a => Comonad ((->) a) where
  extract f = f mempty
  duplicate f = \a b -> f (a <> b)

instance Comonad ((,) w) where
  extract (_, a) = a
  duplicate (w, a) = (w, (w, a))


instance Comonad [] where
  extract x = head x
  duplicate l = init (tails l)


newtype Cont'' m a = Cont'' { runCont'' :: forall r. Cont' r m a }



instance (Monad m, Comonad m) => Comonad (Cont'' m) where
   extract (Cont'' c) = extract . runCont c $ return
   duplicate c = Cont'' (Cont ($c))


data I = I { getI :: forall a. (?inverse :: (a -> a)) => 
                                (a -> a) -> a -> a }
 
zero = I $ \f z -> z
 
instance Eq I where
  I a == I b = a (+1) 0 == b (+1) 0
    where
      ?inverse = (+) (-1)

toInt  (I a) = a (+1) 0
  where
    ?inverse = inv

instance Ord I where
  a `compare` b = toInt a `compare` toInt b
 
instance Num I where
  I a + I b = I $ \f x -> a f (b f x) 
  I a * I b = I $ \f x -> a (b f) x
  negate (I a) = I $ \_ x -> a ?inverse x
  fromInteger x = I $ \f z -> iterate f z !! fromInteger x
  abs x = if x >= 0 then x else  x * (-1)
  signum = undefined
  
  
instance Show I where
  show (I a) = show $ a (+1) 0
    where
     ?inverse = inv
inv = (+) (-1)



type Lens s t a b = forall f. Functor f => (a -> f b) -> (s -> f t) 
type Traversal s t a b = forall f. Applicative f => (a -> f b) -> s -> f t


slice :: (Int, Int) -> Traversal' [a] a
slice (a, b) f s = conc  <$> pure firstBit <*> foldr (liftA2 (:) . f) (pure []) secondBit <*> pure thirdBit  
  where
    conc a b c = a ++ b ++ c
    (firstBit, t) = splitAt a s
    (secondBit, thirdBit) = splitAt b t

_1 :: Lens (a, b) (a, b) a a
_1 f (a, b) = (,b) <$> f a

_2 :: Lens (a, b) (a, b) b b
_2 f (b, a) = (b,) <$> f a


both :: Traversal (a, a) (a, a) a a
both f (a, a') = (,) <$> f a <*> f a'


(^.) :: Lens s t a b -> s -> a
(^.) l s = getConst $ l Const s

(^..) :: Traversal s t a b -> s -> [a]
(^..) l s = getConst $ l (Const . (:[])) s

(%~) :: Traversal s t a b -> (a -> b) -> s -> t
l %~ f = runIdentity . l (Identity . f) 

(.~) :: Traversal s s a a -> a -> s -> s
l .~ a = runIdentity . l (\_ -> Identity a) 


(%=) :: (MonadState s m) => Lens' s a -> (a -> a) -> m ()
(%=) t f = do
  st <- get
  put (t %~ f $ st)

(+=) :: (MonadState s m, Num a) => Lens' s a -> a -> m ()
(+=) t a = t %= (+a)

(+==) :: (MonadState s m, Num a) => Lens' s a -> Lens' s a -> m ()
(+==) t t' = d t' >>= \x -> t += x

(.=) :: MonadState s m => Lens' s a -> a -> m ()
(.=) t a = do
  st <- get
  put (t .~ a $ st)

(.==) :: MonadState s m => Lens' s a -> Lens' s a -> m ()
(.==) t t' = d t >>= \x -> t .= x

(%<) :: (MonadState s m, Ord a) => Lens' s a -> a -> m Bool
l %< a = do
  v <- d l
  return (v < a)

(%>) :: (MonadState s m, Ord a) => Lens' s a -> a -> m Bool
l %> a = do
  v <- d l
  return (v > a)


d :: MonadState s m => Lens' s a -> m a
d t = do
  st <- get
  return (t ^. st)

d' :: MonadState s m => Traversal' s a -> m [a]
d' t = do
  st <- get
  return (t ^.. st)

data Cont' r m a = Cont { runCont :: (a -> m r) -> m r }

type Cont = Cont' ()

instance Functor m => Functor (Cont m) where
  fmap f (Cont g) = Cont $ \h -> g (h . f)


instance Functor m => Applicative (Cont m) where
  pure x = Cont ($x)
  Cont f <*> Cont a = Cont $ \h -> f (\f -> a (\a -> h (f a)))


instance Functor m => Monad (Cont m) where
  return = pure
  Cont c >>= f = Cont $ \h -> c (\a -> runCont (f a) h)

instance MonadTrans Cont where
  lift x = Cont $ \h -> x >>= h

instance Monad m => MonadState s (Cont (StateT s m)) where
   get = lift get
   put = lift . put

instance MonadIO m => MonadIO (Cont' () m) where
  liftIO = lift . liftIO 

data St = St { _i :: Int, _z :: Int, _m :: Int } deriving (Eq, Ord, Show, Read)
i :: Functor f => (Int -> f Int) -> St -> f St
i f (St a b c) = fmap (\x -> St x b c)  (f a)

z :: Functor f => (Int -> f Int) -> St -> f St
z f (St a b c) = fmap (\x -> St a x c)  (f b)

m :: Functor f => (Int -> f Int) -> St -> f St
m f (St a b c) = fmap (\x -> St a b x)  (f c)

while res cond body = do
  b <- cond 
  if not b
  then return res
  else body >>= \x -> while x cond body


(%$) :: (MonadState s m, MonadIO m) => (a -> IO b) -> Lens' s a -> m b
(%$) f l = do
   v <- d l
   liftIO . f $ v

(%$$) :: (MonadState s m, MonadIO m) => ([a] -> IO b) -> Traversal' s a -> m b
(%$$) f l = do
   v <- d' l
   liftIO . f $ v

infixr 1 %$


instance (Applicative f, Num a) => Num (f a) where
    (+) = liftA2 (+)
    (-) = liftA2 (-)
    (*) = liftA2 (*)
    fromInteger = pure . fromInteger
    abs = fmap abs
    signum = fmap signum

if' :: MonadState s m => (m Bool) -> m a -> m a -> m a
if' c t e = do
  cond <- c
  if cond then t else e

--instance (Comonad f, Applicative f, Ord a, Eq a) => Ord (f a) where
--    a <= b = extract $ liftA2 (<=) a b

--callCC :: Monad m => ((a -> Cont m b) -> Cont m a) -> Cont m a
callCC f = Cont $ \h -> runCont (f (\a -> Cont $ \_ -> h a)) h
label a = callCC $ \k -> return (k (return a))

exit :: Monad m => Cont m ()
exit = Cont $ \_ -> return ()

v1 :: Cont (StateT St IO) ()
v1 = do
  i .= 0
  z .= 0
  c <- while (return ()) (i %< 10) $ do
            z +== i
            l1 <- label()
            i += 1
            return l1
  i .= 0
  print %$$ allState
  if' (z %> 100) exit c
  
v2 :: StateT St (Cont Identity) Int
v2 = undefined


type Lens' s a = Lens s s a a
type Traversal' s a = Traversal s s a a

allState :: Traversal' St Int
allState f (St a b c) = St <$> f a <*> f b <*> f c



main = runStateT (runCont v1 return) (St 0 0 0)



-- set
-- traverse
-- filtered




