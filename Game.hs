{-# LANGUAGE Arrows, TupleSections, FlexibleInstances #-}

import Control.Arrow
import Coroutine
import FRP

{- 
We want to make a simple game where there are some dots (circles). Each of these dots move in a fixed path. When the dots collide they bounce off according to the rules of physics. In addition the user manually controls one dot. Up scales the speed vector up, back scales the speed vector down until it reflects it across the axis. Left changes the speed vector by a constant angle in the anti clockwise direction. Right changes the speed vector by a constant angle in the clockwise direction. The bouncing of the dots is effected by the reflection of the speed vector against the tangent to the point of contact between the two dots. 
-}

{-
The position of the dot is the integration of the speed vector. The speed vector is the integration of the acceleration vector. The acceleration vector for all except the users bot is constant except at collisions. And collisions depend on the units positions, thus we have an arrow loop.
-}

type Pos    = (Double, Double)
type Radius = Double
type DotPos = Pos
type Vector a = (a, a)
type Index = Int
type Collision = (Index, Index)
data Dot = Dot Pos (Vector Double)
type Dots = [(Index, Dot)]
data DotConfig = Dc { radius :: Double }

num = 10
rad = 10
config = map (, Dc rad) [1..num]
delta = 0.1


aiDots :: Dots -> Coroutine a Dots
aiDots initialDots
  = loop $ arr snd >>> foldl f (constC []) (zip [1..num] collisionList) >>> withPrevious initialDots
  where
    f ::    Coroutine Dots Dots
         -> (Index, Coroutine (Index, Dots) (Event Collision)) 
         -> Coroutine Dots Dots
    f c (i, c') = c &&& g >>> zipE
      where
        g :: Coroutine Dots Dots
        g =     arr (i,)
            >>> c'
            >>> dotPos (arr collAccVecResolver) initialVel initialPos 
            >>> arr (\(p,v) -> [(i, Dot p v)])
        Just (Dot initialPos initialVel) = lookup i initialDots

dotPos ::  
            Coroutine a (Vector Double) 
        -> (Vector Double) 
        ->  Pos 
        ->  Coroutine a (Pos, Vector Double)
dotPos accVecGen
  = ((accVecGen >>>).). pos


pos ::    (Vector Double) 
          ->  Pos 
          ->  Coroutine (Vector Double) (Pos, Vector Double)
pos initialVel initialPos 
   =     vecIntegrate initialVel 
     >>> vecIntegrate initialPos &&& arr id

--------------------------------------------------------------------------------
-- Physics

collisionList = collisions:collisionList

-- two dots collide if distance between them is less than a delta
collisions :: Coroutine (Index, Dots) (Event Collision)
collisions = arr (\(i, ds) -> flip filter  (map (i,) [1..num]) (filterFunc ds))
  where
    filterFunc :: Dots -> (Int, Int) -> Bool
    filterFunc ds (d, d') = collides (al $ lookup d ds, al $ lookup d' ds)
       where
         collides :: (Dot, Dot) -> Bool
         collides ((Dot (x,y) _), (Dot (x',y') _))
           = (dist (x'-x, y'-y) - rad - rad') < delta
         Just (Dc { radius = rad }) = lookup d config
         Just  (Dc { radius = rad' }) = lookup d' config


collAccVecResolver :: Event Collision -> Vector Double
collAccVecResolver []  = (0,0)
collAccVecResolver _   = undefined 


dist (a,b) = sqrt (a**2 + b**2)



--------------------------------------------------------------------------------
-- | Utilities


al (Just x) = x

allPairs :: [a] -> [(a,a)]
allPairs [] = []
allPairs (a:as) = map (a,) as ++ (allPairs as)

vecIntegrate (x,y) = integrate x *** integrate y