module Machine where

import Control.Applicative
import Control.Comonad

data Moore a b = Moore b (a -> Moore a b)


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
    duplicate m@(Moore b f) = Moore m $  duplicate. f



