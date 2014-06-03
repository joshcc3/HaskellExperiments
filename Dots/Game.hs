{-# LANGUAGE Arrows, TupleSections, FlexibleInstances #-}

module Dots.Game(GameLogic, game) where

import Control.Arrow
import Control.Coroutine
import Control.Coroutine.FRP

import Dots.Keyboard
import Dots.Controls
import Dots.Rect hiding (Pos)


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
type Acceleration = Vector Double
type Velocity = Vector Double
type Index = Int
type Collision = (Index, Index)
data Dot = Dot Pos (Vector Double)
type Dots = [(Index, Dot)]
data DotConfig = Dc { radius :: Double }
type GameLogic = Coroutine Keyboard Rects

num = 3
rad = 10
config = map (, Dc rad) [1..num]
delta = 0.1
initialSit = [(1, Dot (20,20) (10,10)), (2, Dot (10,40) (5, 9)), (3, Dot (50, 50) (4 ,5))]

game :: GameLogic
game 
  = aiDots initialSit >>> arr (\ds -> foldl (\a -> \b -> a ++ [mkRect (snd b)]) [] ds)

mkRect = undefined

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
            >>> c' &&& idC
            >>> dotPos (arr collAccVecResolverC) initialVel initialPos 
            >>> arr (\(p,v) -> [(i, Dot p v)])
        Just (Dot initialPos initialVel) = lookup i initialDots

dotPos ::  
            Coroutine a Acceleration 
        ->  Velocity 
        ->  Pos 
        ->  Coroutine a (Pos, Velocity)
dotPos accVecGen
  = ((accVecGen >>>).). pos


pos ::    Velocity 
          ->  Pos 
          ->  Coroutine (Acceleration) (Pos, Velocity)
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


collAccVecResolverC :: (Event Collision, (Index, Dots)) -> Acceleration
collAccVecResolverC ([],_)  = (0,0)
collAccVecResolverC ((i,i'):cs, (index,ds)) = (xAcc'', yAcc'')
  where
    (xAcc'', yAcc'') = (xAcc + xAcc', yAcc + yAcc')
    (xAcc, yAcc)    = collAccVecResolver (pos, vel) (pos', vel')
    (xAcc', yAcc')   = collAccVecResolverC (cs, (index,ds))
    Just (Dot pos vel) = lookup i ds
    Just (Dot pos' vel') = lookup i' ds


collAccVecResolver :: (Pos, Velocity) -> (Pos, Velocity) -> Acceleration
collAccVecResolver ((x,y), _) ((x',y'), v')
  = project v' normal
  where
    normal = (y - y', x - x)


project (x,y) v = (x' * comp, y' * comp)
   where
     (x', y') = unit v
     comp     = x*x' + y*y'

unit (x,y) = (x / (dist (x, y) ), y/ (dist (x, y)) )

dist (a,b) = sqrt (a**2 + b**2)



--------------------------------------------------------------------------------
-- | Utilities


al (Just x) = x

allPairs :: [a] -> [(a,a)]
allPairs [] = []
allPairs (a:as) = map (a,) as ++ (allPairs as)

vecIntegrate (x,y) = integrate x *** integrate y