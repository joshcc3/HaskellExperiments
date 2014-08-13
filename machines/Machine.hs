{-# LANGUAGE TypeSynonymInstances, RankNTypes, FlexibleInstances #-}

module Machine where

import Control.Applicative
import Control.Comonad
import Prelude hiding ((.), id)
import Data.Monoid
import Control.Monad.Trans.Maybe

data Moore a b = Moore {view :: b, step :: (a -> Moore a b)}

newtype Moore' a b = Moore' (MaybeT (Moore a) b)

instance Functor (Moore a) where
    fmap f (Moore b g) = Moore (f b) (fmap f . g)
        where
            (.) f g x = f (g x)
    
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
        where
        (.) f g x = f (g x)




instance Monoid b => Monoid (Moore a b) where
    mempty = Moore mempty $ const mempty
    mappend m m' = liftA2 mappend m m' 



