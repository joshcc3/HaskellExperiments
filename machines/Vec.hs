{-# LANGUAGE DataKinds, GADTs, KindSignatures, TypeFamilies, UndecidableInstances, MultiParamTypeClasses, FlexibleInstances #-}
module Vec where

import Control.Arrow
import Data.Foldable
import Control.Applicative
import Data.Monoid


data Nat = S Nat | Z 

data Vec :: Nat -> * -> * where
  Cons :: a -> Vec n a -> Vec (S n) a
  Empty :: Vec Z a

head :: Vec (S n ) a -> a
head (Cons x _) = x

tail :: Vec (S n) a -> Vec n a
tail (Cons _ t) = t

type family Add (a :: Nat) (b :: Nat) :: Nat
type instance Add Z n = n
type instance Add (S n) n' = S (Add n n')

type family Mul (a :: Nat) (b :: Nat) :: Nat
type instance Mul Z n = Z
type instance Mul (S n) n' = Add n' (Mul n n')

type family Inf (a :: Nat) :: Nat
type instance Inf Z = S (Inf Z)
type instance Inf (S n) = S (Inf (S n))

append :: Vec n a -> Vec n' a -> Vec (Add n n') a
append Empty x = x
append (Cons a t) v = Cons a (append t v)

cMap :: Vec n a -> (a -> Vec n' b) -> Vec (Mul n n') b
cMap Empty _ = Empty
cMap (Cons a t) f = append (f a) (cMap t f)

instance Foldable (Vec n) where
    fold Empty = mempty
    fold (Cons x t) = mappend x (fold t)

instance Functor (Vec n) where
    fmap f (Cons x t) = Cons (f x) (fmap f t)
    fmap f Empty = Empty




class Distribute' (n :: Nat) (a :: * -> * -> *) where
    distr :: a b (Vec n c) -> Vec n (a b c)
            
instance (Arrow a) => Distribute' Z  a where
  distr a = Empty

instance (Arrow a, Distribute' n a) => Distribute' (S n) a where
    distr a = Cons (a >>> arr Vec.head) (distr (a >>> arr Vec.tail))

