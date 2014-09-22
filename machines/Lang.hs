{-# LANGUAGE RankNTypes #-}

module Lang where

import Data.Monoid
import Control.Monad.Logic
import Control.Monad.State
import Data.Sequence
import Pipes

type Zip a = (Seq a, a, Seq a)



type Zip' m a = m (Seq a, a, Seq a, Producer a m ())

forward' :: Monad m => Zip' m a -> Zip' m a
forward' m = do
  (s, a, s', p) <- m
  case viewl s' of
    EmptyL -> next p >>= \x -> case x of
                               Left _ -> error "Stream dried up"
                               Right (a', p'') -> return (s |> a, a', s', p'')
    (a' :< s'') -> return (s |> a, a', s'', p)

-- Zip = Zip' Identity, change to use this

forward :: Zip a -> Zip a
forward (s, a, s') = case (viewl s') of
                    EmptyL -> error "Stream dried up"
                    (a' :< s'') -> (s |> a, a', s'')

view' :: Monad m => Zip' m a -> m a
view' m = m >>= \(_, a, _, _) -> return a

view :: Zip a -> a
view (_, a, _) = a

type Reg a b = StateT (Zip a) Logic b


match :: Eq a => a -> b -> Reg a b
match a b = do
  s <- get
  if (view s) == a then put (forward s) >> return b else mzero



conc :: Monoid b => Reg a b -> Reg a b -> Reg a b
conc r r' = do
  b <- r
  b' <- r'
  return (b <> b')
  
alter :: Monoid b => Reg a b -> Reg a b -> Reg a b
alter = interleave

star :: Monoid b => Reg a b -> Reg a b
star m = m `alter` (m `conc` star m)

tag :: Monoid b => b -> String -> [(Char, b)]
tag final s = Prelude.zip (init s) (repeat mempty) ++ [(last s, final)]

toRegex :: (Monoid b, Eq a) => [(a, b)] -> Reg a b
toRegex = foldl1 conc . map (uncurry match)

runReg :: Monoid c => ((b, Zip a) -> c) -> Zip a -> Reg a b -> c
runReg f initS r = mconcat . fmap f $ observeAll $ runStateT r initS

runReg' :: (Monoid b, Eq a) => Zip a -> Reg a b -> [b]
runReg' initS r = fmap fst $ observeAll $ runStateT r initS


--ifR = toRegex $ tag "IF" "if" 
--forR = toRegex $ tag "FOR" "for"
