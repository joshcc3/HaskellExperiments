{-# LANGUAGE DataKinds, GADTs, KindSignatures, TypeFamilies #-}
module Vec where

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

append :: Vec n a -> Vec n' a -> Vec (Add n n') a
append Empty x = x
append (Cons a t) v = Cons a (append t v)

instance Foldable (Vec n) where
    fold Empty = mempty
    fold (Cons x t) = mappend x (fold t)

instance Functor (Vec n) where
    fmap f (Cons x t) = Cons (f x) (fmap f t)
    fmap f Empty = Empty

