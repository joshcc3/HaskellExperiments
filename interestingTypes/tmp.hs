{-# LANGUAGE ExistentialQuantification #-}
module Tmp where

import Control.Monad.Cont
import Control.Monad.Identity

data F a = Fold { unfold :: F a -> a }

g :: F a -> a
g f = unfold f f


-- a continuation is waiting for a function to consume its partially computer value


{-
So lets consider a specific use case.
We want to create a compiler for lazy evaluation.
The user can perform the following expressions the lazy natural numbers

max :: Nat -> Nat -> Nat
min :: Nat -> Nat -> Nat
lToNat :: [a] -> Nat
iToNat :: Int -> Nat

We will represent natural numbers with continuations:
Nat a = (a -> Int) -> Int

We may also want to inspect the value of the int, but we'll leave that for a later time.

-}
{-
class LNat a where
    maxN :: LNat a => a -> a -> a 
    minN :: LNat a => a ->  a -> a
    lToa :: LNat a => [a] -> b
    iToa :: LNat a => Int -> a
-}



data Nat = forall r. N (Cont r Closure)

data Val = NegInf | Val Int | PosInf deriving (Eq, Ord, Show)

data Closure = V Int | Thunk Nat Val

iToNat :: Int -> Nat
iToNat a = N $ return (V $ Val a)

lToNat :: Int -> [a] -> Nat
lToNat v [] = iToNat v
lToNat v (_:ls) = N $ return $ Thunk (lToNat (v+1) ls) (Val (v+1))

maxN :: Nat -> Nat -> Nat
maxN (N n) (N n') = do
  (v, c) <- n
  (v', c') <- n'
  




{-
So for a natural number we only want to evaluate as much as is required.
A
-}

