{-# LANGUAGE TupleSections, FlexibleInstances #-}

module Dots.Utils where

import Dots.Physics
import Control.Coroutine
import Control.Arrow
import Control.Coroutine.FRP

toDots :: (Index, (DotPos, Velocity)) -> Map Index (DotPos, Velocity)
toDots = (:[])

al (Just x) = x

allPairs :: [a] -> [(a,a)]
allPairs [] = []
allPairs (a:as) = map (a,) as ++ (allPairs as)

vecIntegrate (x,y) = integrate x *** integrate y


toState :: Map Index (Dot a) -> State a
toState d = State { dots = d }


instance Num a => Num (a, a) where
  (+) (a,a') (b, b') = (a+b, a'+b')
  (*) (a, a') (b,b') = (a*b, a'*b')
  abs v = undefined 
  signum (a, a') = undefined
  fromInteger i = undefined

dotRadius :: a -> Radius
dotRadius = undefined 

dotPosition :: a -> DotPos
dotPosition = undefined

dotVelocity :: a -> Velocity
dotVelocity = undefined

{-
we wnat to build our object incrementally by concatenating properties. We want to build the properties by concatenating the values of the properties. The values of the properties simply exist. 

Our builder will represent the object as a map from enumerable properties to values.

concatentation of two builders is simply the concatenation through zipping of the values of properties.

For Dot - 
Examples of values of properties:
Property: radius, value: 1

concatenating values:
given (d,property) (d', property')

Enumerate the properties.


we can build our object from a map. We first enumerate the properties. Build the enumerated version of the object.

concatFeature :: Last a => () -> Dot a -> Dot a -> Dot a
concatFeature f d d' = Dot (f d' <> f d)

setRadius (Dot a b c d e _) r = Dot a b c d e r

So what is a builder. a builder takes some property of the object it is building and appends that property to the object. Function from property, to an incomplete object. An incomplete object is a sequence of build instructions.
Builder a :: Property a -> a
Property a = 

data Dot a = Dot { radius :: Int, position :: DotPos, velocity :: Velocity, physics :: Physics a }
type GameLogic = Coroutine Keyboard Rects
type Physics a = [Coroutine (a, State a) Acceleration]
data State a = State { dots :: Map Index (Dot a) }
-}

{-
Think about implementing a builder as a monoid. 
-}




{- 
A functor maps objects in a category C to objects in a category D and morphisms in a category C to morphisms in a category D. 
For functors F and G, A natural transformation n is a family of morphisms. These include morphisms that relate F(X) -> G(X), that is associate with X from the category C a morphism in category D. And for each morphism f: X -> Y, in C, F(f) o nY = nX o G(f)
-}


{- So what we want to do is create a reader functor. that is a functor that lifts morphisms in the base category to morphisms in the reader category. -}

{-

data StateFunctor s a b c = StateF { runState :: a (b,s) (c,s) }


instance Arrow a => Functor (StateFunctor s a b) where 
  fmap f = StateF $ arr f *** returnA


instance Arrow a => Category (StateFunctor s a) where
  
  StateF s . State s' = 

instance Arrow a => Arrow (StateFunctor s a) where
  arr = fmap
--  first :: StateFunctor s a b c -> StateFunctor s a (b,d) (c,d) 
  first (StateF s) = StateF $ swap >>> s *** returnA >>> swap
    where
      swap = arr (\((a,b) ,c) -> ((a, c) ,b))
-}


