{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Rank2Types #-}

module Machines.Prelude.Types where

import Machines.Mon

data Nat = S Nat | Z

type family (:.:) (a :: * -> *) (b :: * -> *) (c :: *)
type instance (:.:) a b c = a (b c)


f :: ((a, a) -> b) -> a -> a -> b
f = curry

--f' :: (a, (a, a)) -> b -> (a -> a -> a -> b)
f' :: ((a1, (a, b)) -> c) -> a1 -> a -> b -> c
f' inp = fmap curry . curry $ inp

--f'' :: (a, (a, (a, a))) -> b -> (a -> a -> a -> a)
f'' :: ((a2, (a1, (a, b))) -> c) -> a2 -> a1 -> a -> b -> c
f'' inp = (fmap . fmap) curry . fmap curry . curry $ inp



pow :: (a -> a) -> Int -> a -> a
pow f 0 = id
pow f n = f . pow f (n - 1)

type family FunctorStream (f :: * -> *) (n :: Nat) a
type instance FunctorStream f Z a = a
type instance FunctorStream f (S n) a = f (FunctorStream f n a)

type ReflectedNat n = forall a. Const a n

class ToNum (n :: Nat) where
    toNum :: Const a n -> Int

instance ToNum Z where
    toNum _ = 0

instance ToNum n => ToNum (S n) where
    toNum (Const x :: Const a (S n)) = 1 + toNum (Const x :: Const a n)

