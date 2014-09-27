{-# LANGUAGE RankNTypes #-}

module Traversal where

import Control.Applicative
import Data.Monoid

type Traversal s t a b = Applicative f => (a -> f b) -> s -> f t

mapMOf :: (Monad m, Applicative m) => Traversal s t a b -> (a -> m b) -> s -> m t
mapMOf t = t

mapAccumROf :: Monoid acc => Traversal s t a b -> (acc -> a -> (acc, b)) -> acc -> s -> (acc, t)
mapAccumROf t f acc = t (f acc)


