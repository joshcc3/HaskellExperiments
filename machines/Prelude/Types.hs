{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
--{-# LANGUAGE #-}

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



{-
instance PairStreamFunc Z a where
    psf f b = f b

instance PairStreamFunc (S n) a where
    psf f b = 
-}