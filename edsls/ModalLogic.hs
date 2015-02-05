
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module ModalLogic where

import Data.Functor.Foldable


data ModalLogicF a n = Id a | Not n | n :|| n | n :&& n | Box n | TrueM
                     deriving (Functor, Eq, Ord, Show)

type ModalLogic a = Fix (ModalLogicF a)

type KripkeF a = ([a], Set (a, a))

type KripkeM p a = (KripkeF a, p -> Set a)

type Property a = ModalLogic a

diamond :: ModalLogic a -> ModalLogic a
diamond = Fix . Not . Fix . Box . Fix . Not

false :: ModalLogic a
false = Fix $ Not (Fix TrueM)

(|=) :: forall p a. (KripkeM p a, a) -> Property p -> Bool
(m@((universe, r), ef), w) |= property = para alg property
  where
    alg :: ModalLogicF p (Property p, Bool) -> Bool
    alg TrueM = True
    alg (Id p) = ef p w
    alg (Not (_, n)) = not n
    alg ((_, n) :|| (_, n')) = n || n'
    alg ((_, n) :&& (_, n')) = n && n'
    alg (Box (n, _)) = all (\w' -> not (r (w, w')) || (m, w') |= n) universe


-- this is the meaning of a set
type Set a = a -> Bool


data TemporalF a n = IdT a | NotT a | AndT n n | G n | X n | U n n

type Temporal a = Fix (TemporalF a)



