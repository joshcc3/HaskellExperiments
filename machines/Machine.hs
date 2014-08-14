{-# LANGUAGE TypeSynonymInstances, RankNTypes, FlexibleInstances #-}

module Machine where

import Control.Applicative
import Control.Comonad

import Data.Monoid
import Control.Monad.Trans.Maybe


data Moore a b = Moore b (a -> Moore a b)

view :: Moore b c -> c
view (Moore a _) = a

-- | A source is a machine that is irrespective of the input
type Source b = forall a. Moore a b


instance Functor (Moore a) where
    fmap f (Moore b g) = Moore (f b) (fmap f . g)
    
instance Applicative (Moore a) where
    pure x = Moore x $ const $ pure x
    Moore b f <*> Moore b' g = Moore (b b') $ \a -> f a <*> g a

instance Monad (Moore a) where
    return = pure
    Moore b f >>= g = Moore b' $ \a -> f a >>= g
        where
          Moore b' f' = g b


instance Comonad (Moore a) where
    extract (Moore b _) = b
    duplicate m@(Moore b f) = Moore m $  duplicate . f



instance Monoid b => Monoid (Moore a b) where
    mempty = Moore mempty $ const mempty
    mappend m m' = liftA2 mappend m m' 

yield :: c -> Source (Maybe c)
yield c = Moore (Just c) $ const $ pure Nothing

stream :: [c] -> Source (Maybe c)
stream = foldl1 (>>) . map yield

