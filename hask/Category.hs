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

import Prelude (Either(..), ($))

type family (~>) :: i -> i -> *
type instance (~>) = (->)
type instance (~>) = Nat

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