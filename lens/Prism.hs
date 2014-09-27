{-# LANGUAGE RankNTypes #-}
module Prism where

import Control.Applicative
import Data.Profunctor

type Prism s t a b = (Choice p, Applicative f) => p a (f b) -> p s (f t)

prism :: (b -> t) -> (s -> Either t a) -> Prism s t a b
prism f g = dimap g h . right'
    where 
      h (Left x) = pure x
      h (Right x) = fmap f x

      

_Left :: Prism (Either a c) (Either b c) a b
_Left = prism Left g
    where 
      g :: Either a c -> Either (Either b c) a
      g (Left a) = Right a
      g (Right c) = Left (Right c)

_Right :: Prism (Either c a) (Either c b) a b
_Right = prism Right g
    where 
      g :: Either c a -> Either (Either c b) a
      g (Left c) = Left (Left c)
      g (Right a) = Right a

