{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Type where

import Data.Profunctor

type Simple f s a = f s s a a

class Wrapped s t a b 

data Exchange a b s t = Exchange (s -> a) (b -> t)

instance Profunctor (Exchange a b) where
    dimap f g (Exchange f' g') = Exchange (dimap f id f') (fmap g g')


type LensLike f s t a b = (a -> f b) -> s -> f t

newtype Pretext p a b t = Pretext { runPretext :: Functor f => p a (f b) -> f t }

