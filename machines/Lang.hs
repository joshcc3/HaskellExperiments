{-# LANGUAGE RankNTypes #-}

module Lang where

import Data.Monoid
import Control.Monad.Logic
import Control.Monad.State
import Data.Sequence
import Pipes
import Control.Monad.Identity

type Zip' m a = m (Seq a, a, Seq a, Producer a m ())

type Zip a = Zip' Identity a

forward :: Monad m => Zip' m a -> Zip' m a
forward m = do
  (s, a, s', p) <- m
  case viewl s' of
    EmptyL -> next p >>= \x -> case x of
                               Left _ -> error "Stream dried up"
                               Right (a', p'') -> return (s |> a, a', s', p'')
    (a' :< s'') -> return (s |> a, a', s'', p)

view :: Monad m => Zip' m a -> m a
view m = m >>= \(_, a, _, _) -> return a

view' :: Zip a -> a
view' = runIdentity . view

type Reg m a b = StateT (Zip' m a) (LogicT m) b

match :: (Monad m, Eq a) => a -> b -> Reg m a b
match a b = do
  s <- get
  inp <- (lift . lift) (view s)
  if inp == a then put (forward s) >> return b else mzero

conc :: (Monoid b, Monad m) => Reg m a b -> Reg m a b -> Reg m a b
conc r r' = do
  b <- r
  b' <- r'
  return (b <> b')
  
alter :: (Monoid b, Monad m) => Reg m a b -> Reg m a b -> Reg m a b
alter = interleave

star :: (Monoid b, Monad m) => Reg m a b -> Reg m a b
star m = m `alter` (m `conc` star m)

tag :: Monoid b => b -> String -> [(Char, b)]
tag final s = Prelude.zip (init s) (repeat mempty) ++ [(last s, final)]

toRegex :: (Monoid b, Eq a, Monad m) => [(a, b)] -> Reg m a b
toRegex = foldl1 conc . map (uncurry match)

runReg :: (Monoid c, Functor m, Monad m) => ((b, Zip' m a) -> c) -> Producer a m () -> Reg m a b -> m c
runReg f p r = fmap (mconcat . fmap f) $ observeAllT $ runStateT r (next p >>= \x -> case x of
                                                                             Left _ -> error "Empty Stream"
                                                                             Right (a, p') -> return (fromList [], a, fromList [], p'))

runReg' :: (Monoid b, Eq a, Functor m, Monad m) => Producer a m () -> Reg m a b -> m [b]
runReg' p r = runReg (\(b, _) -> [b]) p r


--ifR = toRegex $ tag "IF" "if" 
--forR = toRegex $ tag "FOR" "for"
