{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures, TypeFamilies, UndecidableInstances, MultiParamTypeClasses, FlexibleInstances, PolyKinds, TypeOperators #-}

module Machines.Vec where

import Machines.Prelude.Types
import Control.Arrow
import Data.Foldable
import Control.Applicative
import Data.Monoid

data Vec :: Nat -> * -> * where
  Cons :: a -> Vec n a -> Vec (S n) a
  Empty :: Vec Z a


instance VecFns Z where
    initV _ = Empty
    lastV (Cons x _) = x
    headV (Cons x _) = x
    tailV _ = Empty

instance (VecFns n) => VecFns(S n) where
    initV (Cons x v) = Cons x (initV v)
    lastV v = lastV $ tailV v
    headV (Cons x _) = x
    tailV (Cons x v) = v

class VecFns (n :: Nat) where
    initV :: Vec (S n) a -> Vec n a
    lastV :: Vec (S n) a -> a
    headV :: Vec (S n) a -> a
    tailV :: Vec (S n) a ->Vec n a

append :: Vec n a -> Vec n' a -> Vec (Add n n') a
append Empty x = x
append (Cons a t) v = Cons a (append t v)

data Suc n = Suc n
data Zero = Zero

type family Morph a :: Nat
type instance Morph Zero = Z
type instance Morph (Suc n) = S (Morph n)

class Repl n where
    repl :: n -> a -> Vec (Morph n) a

instance Repl Zero where
    repl Zero a = Empty

instance (Repl n) => Repl (Suc n) where
    repl (Suc n) a = Cons a (repl n a)

cMap :: Vec n a -> (a -> Vec n' b) -> Vec (Mul n n') b
cMap Empty _ = Empty
cMap (Cons a t) f = append (f a) (cMap t f)


instance Foldable (Vec n) where
    fold Empty = mempty
    fold (Cons x t) = mappend x (fold t)

instance Functor (Vec n) where
    fmap f (Cons x t) = Cons (f x) (fmap f t)
    fmap f Empty = Empty

distr :: Arrow a => Vec n (a b c) -> a b (Vec n c)
distr Empty = arr $ const Empty
distr (Cons x t) = x &&& distr t >>> arr (uncurry Cons)

class Distribute' (n :: Nat) (a :: * -> * -> *) where
    distr' :: a b (Vec n c) -> Vec n (a b c)
            
instance (Arrow a) => Distribute' Z  a where
  distr' a = Empty

instance (Arrow a, Distribute' n a, VecFns n) => Distribute' (S n) a where
    distr' a = Cons (a >>> arr headV) (distr' (a >>> arr tailV))

type family Add (a :: Nat) (b :: Nat) :: Nat
type instance Add Z n = n
type instance Add (S n) n' = S (Add n n')

type family Mul (a :: Nat) (b :: Nat) :: Nat
type instance Mul Z n = Z
type instance Mul (S n) n' = Add n' (Mul n n')


