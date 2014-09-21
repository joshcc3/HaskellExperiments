{-# LANGUAGE TypeOperators #-}

module Lang where

import Data.Monoid
import Control.Monad.Logic
import Control.Monad.State
import Data.Sequence


type Zip a = (Seq a, a, Seq a)

next :: Zip a -> Zip a
next (s, a, s') = case (viewl s') of
                    EmptyL -> error "Stream dried up"
                    (a' :< s'') -> (s |> a, a', s'')

prev :: Zip a -> Zip a
prev (s, a, s') = case (viewr s) of
                    EmptyR -> error "Beyond start of stream"
                    (s'' :> a') -> (s'' |> a, a', s')

view :: Zip a -> a
view (_, a, _) = a

type Reg a b = StateT (Zip a) Logic b

match :: Eq a => a -> b -> Reg a b
match a b = do
  s <- get
  if (view s) == a then put (next s) >> return b else mzero

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

