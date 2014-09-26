{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
--{-# LANGUAGE ExistentialQuantification #-}
--{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
--{-# LANGUAGE LambdaCase #-}

module Iso where

import Equality
import Data.Profunctor
import Type
import Control.Monad.Identity

type Iso s t a b = (Profunctor p, Functor f) => p a (f b) -> p s (f t)

type AnIso s t a b = Exchange a b a (Identity b) -> Exchange a b s (Identity t)

-- s -> a, b -> t
-- a -> s, t -> b
-- a ~ b, t ~ s

--anIso :: Equality a s b t -> AnIso a b s t
--anIso p (Exchange f g) = substEq 

{-
Constructu
-}
iso :: (s -> a) -> (b ->t) -> Iso s t a b
iso f g p = dimap f (fmap g) p

from :: Iso s t a b -> Iso b a t s
from f' = g $ f' (Exchange Prelude.id Identity :: Exchange a b a (Identity b))
    where 
      g :: Exchange a b s (Identity t) -> Iso b a t s
      g (Exchange f h) = iso (runIdentity . h) f

-- f    :: p a (f b) -> p s (f t)
-- (a -> s) (t -> b)
-- s -> a, b -> t
-- psft :: p s (f t)
-- pafb :: p a (f b)

wrapping :: Wrapped s s a a => (s -> a) -> Iso s s a a
wrapping = undefined

enum :: Enum a => Simple Iso Int a
enum = iso toEnum fromEnum

simple :: Simple Iso a a
simple = iso Prelude.id Prelude.id

mapping :: Functor f => Iso s t a b -> Iso (f s) (f t) (f a) (f b)
mapping = undefined

curried :: Iso ((a, b) -> c) ((d, e) -> f) (a -> b -> c) (d -> e -> f)
curried = undefined

uncurried :: Iso (a -> b -> c) (d -> e -> f) ((a, b) -> c) ((d, e) -> f)
uncurried = undefined
