{-# LANGUAGE TypeSynonymInstances, RankNTypes, FlexibleInstances #-}

module Machine where

import Control.Applicative
import Control.Comonad

import Data.Monoid
import Control.Monad.Trans.Maybe


data Moore a b = Moore {view :: b, runMoore :: (a -> Moore a b)}

-- | A source is a machine that is irrespective of the input
type Source b = forall a. Moore a b


instance Functor (Moore a) where
    fmap f (Moore b g) = Moore (f b) (fmap f . g)
    
instance Applicative (Moore a) where
    pure x = Moore x $ const $ pure x
    Moore b f <*> Moore b' g = Moore (b b') $ \a -> f a <*> g a

instance Monad (Moore a) where
    return = pure
    k >>= f = j (fmap f k) where
        j (Moore a g) = Moore (extract a) (\x -> j $ fmap (\(Moore _ h) -> h x) (g x))
-- this bind works on as follows, first a machine of a machine is created with the fmap such that its like the second machine which
-- is a reader on the first is sortve composed with the first. Each step of the machine, will progress k along one of its branches 
-- and simulateously along each of the sub branches progress the machine at those points using the same input.

instance Comonad (Moore a) where
    extract (Moore b _) = b
    duplicate m@(Moore b f) = Moore m $  duplicate . f



yield :: c -> Source (Maybe c)
yield c = Moore (Just c) $ const $ pure Nothing

stream :: [c] -> Source (Maybe c)
stream = foldl1 (>>) . map yield
