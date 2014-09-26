{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}

module Equality where

import Control.Monad.Identity

{-
Equality s t a b is a witness that s ~ a and b ~ t.
What we mean by a witness is that, if there is a function that can go from
p a (f b) -> p s (f t), forall p and f, it is not possible unless the types are equal.
the existence of such a function implies equality and the function is the witness.
-}
type Equality s t a b = forall p f. p a (f b) -> p s (f t)

-- Identical s t a b is a type with the only inhabitant, of type Identical a b a b
data Identical s t a b where
    Identical :: Identical a b a b

{-

One witness is AnEquality.
We have to define mapEq.
mapEq says: If we have a witness that proves that (s ~ a, b ~ t), then we can perform the substitution s -> a in any context, f i.e. f s -> f a

-}
type AnEquality s t a b = Identical a (Identity b) a (Identity b) -> Identical a (Identity b) s (Identity t)

id :: Equality a b a b
id x = x

{-
Thus in order to mapEq, we just substitute a for s in f s which we can only do if the context s ~ a is satisfied, which thus makes it fall right out of substEq.
-}
mapEq :: AnEquality s t a b -> f s -> f a
mapEq proof fs = substEq proof fs

{-
Right, now the big one 'substEq'. What substEq says is that, if we have a proof that s ~ a, and b ~ t then we can create an r that can only be created if this constraint is satified. The challenge with this function is convincing the type checker of the equality using data. This construction, the AnEquality proves it. The reason we get confused with such a thing is that we are assuming this to be forall s t a b. The truth is, this proof will only work when we are  given a valid proof. A valid proof has to have the form (Identical -> Identical) because a proof is only valid if the types of its arguments are inhabitated. 
-}
substEq :: AnEquality s t a b -> ((s ~ a, t ~ b) => r) -> r
substEq f r = case (f Identical) of
                Identical -> r


{-
What fromEq says is, if we have the proof that s ~ a and b ~ t, then we have the proof that b ~ t and s ~ a.
-}
fromEq :: AnEquality s t a b -> AnEquality b a t s
fromEq f = substEq f (\Identical -> Identical)
{-  where
    f' :: (s ~ a, t ~ b) => Identical t (Identity s) t (Identity s) -> Identical t (Identity s) b (Identity a)
    f' Identical = Identical
-}
-- 



--g :: p a (f b) -> p s (f t)
--x :: p t (f s) -> p b (f a)

--a -> b -> f b -> f a

