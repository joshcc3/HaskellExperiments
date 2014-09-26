{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
--{-# LANGUAGE ExistentialQuantification #-}
--{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
--{-# LANGUAGE LambdaCase #-}

module Iso where

import Data.Profunctor
import Type
import Control.Monad.Identity

type Iso s t a b = (Profunctor p, Functor f) => p a (f b) -> p s (f t)

type AnIso s t a b = Exchange a b a (Identity b) -> Exchange a b s (Identity t)

type Iso' s a = Iso s s a a

-- s -> a, b -> t
-- a -> s, t -> b
-- a ~ b, t ~ s

--anIso :: Equality a s b t -> AnIso a b s t
--anIso p (Exchange f g) = substEq 

{-
Constructu
-}
iso' :: Iso s t a b -> ((s -> a), (b -> t))
iso' is = case is (Exchange Prelude.id Identity) of
            Exchange s b -> (s, runIdentity . b)

iso :: (s -> a) -> (b ->t) -> Iso s t a b
iso f g p = dimap f (fmap g) p

from :: Iso s t a b -> Iso b a t s
from f = uncurry (\g h -> iso h g) $ iso' f


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
mapping = uncurry (\g h -> iso (fmap g) (fmap h)) . iso' 

non :: Eq a => a -> Iso' (Maybe a) a
non = undefined

anon :: a -> (a -> Bool) -> Iso' (Maybe a) a
anon a f = iso g (\a' -> if f a' then Nothing else Just a)
    where 
      g (Just x) = x
      g Nothing = a
                         
curried :: Iso ((a, b) -> c) ((d, e) -> f) (a -> b -> c) (d -> e -> f)
curried = iso curry uncurry

uncurried :: Iso (a -> b -> c) (d -> e -> f) ((a, b) -> c) ((d, e) -> f)
uncurried = iso uncurry curry

au :: Iso s t a b -> ((s -> a) -> e -> b) -> e -> t
au is f = uncurry (\g h -> fmap h (f g)) $ iso' is

auf :: Iso s t a b -> ((r -> a) -> e -> b) -> (r -> s) -> e -> t
auf is f g = uncurry (\s b -> b . f (s . g)) $ iso' is


{-

h :: s -> a
k :: b -> t
f :: ((r -> a) -> e -> b)
g :: b -> s
e :: e

-}


under :: Iso s t a b -> (t -> s) -> b -> a
under is f b = uncurry (\h k -> h $ f $ k b) $ iso' is
