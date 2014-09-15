{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ConstraintKinds #-}

import Prelude (Either(..), ($), undefined, Ord(..), Eq(..))

type family (~>) :: i -> i -> *
type instance (~>) = (->)
type instance (~>) = Nat
type instance (~>) = (:-)

class (~>) ~ h => Category h where
    id :: h a a
    (.) :: h b c -> h a b -> h a c

instance Category (->) where
    id a = a
    (.) f g a = f (g a)

-- phi : Nat - phi . fmap f = fmap f . phi

newtype Nat f g = Nat { runNat :: forall a. f a ~> g a }




instance Category ((~>) :: i -> i -> *) => Category (Nat :: (k -> i) -> (k -> i) -> *) where
    id = Nat id
    (.) (Nat f) (Nat g) = Nat (f . g)

class Functor f where
    fmap :: a ~> b -> f a ~> f b

instance Functor (Either a) where
    fmap f (Left x) = Left x
    fmap f (Right x) = Right (f x)

-- Either :: * -> * -> *
-- Functor Either :: * -> (* -> *)
-- Either is a functor from the category of haskell types to the category with functors as objects
-- thus morphisms are natural transformations (categories are defined by their natural transformations)

instance Functor Either where
    fmap f = Nat $ \case
             Left x -> Left (f x)
             Right x -> Right x

-- Dict :: Constraint -> *
data Dict p where
    Dict :: p => Dict p


-- (:-) :: Constraint -> Constraint -> *
-- Sub :: c => (c' => Dict c')
newtype (:-) c c' = Sub { imply :: c => Dict c' }

-- constraint a is established globally, constraint b is established by pattern matching
-- we are only able to create Dict b is the constraint b is established.
-- a => (b => Dict b), since is established, pattern matching on Dict
-- enforces the constraint b, thus satisfying b => r
(//) :: a => (b => r) -> a :- b -> r
(//) r (Sub Dict) = r

instance Category (:-) where
--    id :: (:-) a a
      id = Sub Dict
      f . g = Sub $ Dict // f // g
--    f . g :: b => (c => Dict c) -> a => (b => Dict b) -> a => (c => Dict c)
--    b => (c => Dict c) -> 

instance Functor Dict where
--  fmap :: a ~> b -> Dict a ~> Dict b
    fmap x y = case (y, x) of
                      (Dict, Sub Dict) -> Dict
-- To establish the constraint to prove Dict b, a must be established first
-- however the type checker establishes constraints sequentially
-- thus we flip the arguments establishing a first and then x

proof :: Ord a :- Eq a
proof = Sub Dict
-- this holds because Ord a :- Eq a desugars to Ord a => Dict (Eq a),
-- thus the only context available to create objects of Dict is Ord a,
-- however in order to create objects of Dict (Eq a) we require the 
-- context Eq a, however the type checker already knows that Ord a 
-- implies Eq a and this type checks

-- cannot be proven as Eq is not sufficient to show Ord
invalid :: Eq a :- Ord a
invalid = undefined
