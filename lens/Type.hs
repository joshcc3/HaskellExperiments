{-# LANGUAGE MultiParamTypeClasses #-}
module Type where

import Data.Profunctor

type Simple f s a = f s s a a

class Wrapped s t a b 

data Exchange a b s t = Exchange (s -> a) (b -> t)

instance Profunctor (Exchange a b) where
    dimap f g (Exchange f' g') = Exchange (dimap f id f') (fmap g g')
