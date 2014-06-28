{-# LANGUAGE TupleSections, FlexibleInstances #-}

module Dots.Utils where

import Dots.Physics
import Control.Coroutine
import Control.Arrow
import Control.Coroutine.FRP
import qualified Data.Map as M
import Dots.Rect

toDots :: (Index, (DotPos, Velocity)) -> M.Map Index (DotPos, Velocity)
toDots = M.fromList . (:[])

al (Just x) = x

allPairs :: [a] -> [(a,a)]
allPairs [] = []
allPairs (a:as) = map (a,) as ++ (allPairs as)

vecIntegrate (x,y) = integrate x *** integrate y


toState :: M.Map Index (Dot a) -> State a
toState d = State { dots = d }


instance Num a => Num (a, a) where
  (+) (a,a') (b, b') = (a+b, a'+b')
  (*) (a, a') (b,b') = (a*b, a'*b')
  abs v = error "Abs not defined" 
  signum (a, a') = error "Signum not defined"
  fromInteger i = (fromInteger i, 0)



dotsToRect :: M.Map Index (Dot a) -> Shapes
dotsToRect m = M.foldl func [] m
  where
    func b a = mkRect a : b

dotsToCircle :: M.Map Index (Dot a) -> Shapes
dotsToCircle m = M.foldl func [] m
  where
    func b a = mkCircle a : b

mkRect Dot{radius = r, position = (x, y)} 
  = Rectangle (x-r,y-r) (2*r,2*r)

mkCircle Dot{radius = r, position = p}
  = Circle p r
{- 
A functor maps objects in a category C to objects in a category D and morphisms in a category C to morphisms in a category D. 
For functors F and G, A natural transformation n is a family of morphisms. These include morphisms that relate F(X) -> G(X), that is associate with X from the category C a morphism in category D. And for each morphism f: X -> Y, in C, F(f) o nY = nX o G(f)
-}


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


