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

c :: Monoid b => Reg a b -> Reg a b -> Reg a b
c r r' = do
  b <- r
  b' <- r'
  return (b <> b')
  
a :: Monoid b => Reg a b -> Reg a b -> Reg a b
a = interleave

star :: Monoid b => Reg a b -> Reg a b
star m = m `a` (m `c` star m)

ifR :: Reg Char String
ifR = match 'i' "I" `c` match 'f' "F"

forR :: Reg Char String
forR = match 'f' "F" `c` match 'o' "O" `c` match 'r' "R"

foe :: Reg Char String
foe = match 'f' "F" `c` match 'o' "O" `c` match 'e' "E"

r1 = star forR

trial initS r = observe $ runStateT r initS
