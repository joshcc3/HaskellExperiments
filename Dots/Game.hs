{-# LANGUAGE Arrows, TupleSections, FlexibleInstances #-}

module Dots.Game where

import Control.Arrow
import Control.Coroutine
import Control.Coroutine.FRP

import Data.Bifunctor (bimap)

import Dots.Keyboard
import Dots.Controls
import Dots.Rect hiding (Pos)

import Data.List

{- 
We want to make a simple game where there are some dots (circles). Each of these dots move in a fixed path. When the dots collide they bounce off according to the rules of physics. In addition the user manually controls one dot. Up scales the speed vector up, back scales the speed vector down until it reflects it across the axis. Left changes the speed vector by a constant angle in the anti clockwise direction. Right changes the speed vector by a constant angle in the clockwise direction. The bouncing of the dots is effected by the reflection of the speed vector against the tangent to the point of contact between the two dots. 
-}

{-
The position of the dot is the integration of the speed vector. The speed vector is the integration of the acceleration vector. The acceleration vector for all except the users bot is constant except at collisions. And collisions depend on the units positions, thus we have an arrow loop.
-}

type Pos    = (Int, Int)
type Radius = Int
type DotPos = Pos
type Vector a = (a, a)
type Acceleration = Vector Int
type Velocity = Vector Int
type Index = Int
type Collision = (Index, Index)
data Dot = Dot Pos Velocity deriving (Show)
type Dots = [(Index, Dot)]
data DotConfig = Dc { radius :: Int }
type GameLogic = Coroutine Keyboard Rects
type Physics a = [Coroutine a Acceleration]

num = 3
rad = 20
config = map (, Dc rad) [1..num]
delta = 2

initialSit = [(1, Dot (100,100) (0,6)), (2, Dot (100,160) (0, 3)), (3, Dot (500, 300) (3,1))]

physics :: [(Index, Physics a)]
physics = [] -- replicate num (1, [dotCollision])

{-
game :: GameLogic
game = constC initialSit >>> c >>> (arr $ foldl (\a -> \b -> a ++ [mkRect b]) [])
  where
    incPos (i, Dot (x,y) v) = (i, Dot(x+1, y+1) v)
    c = Coroutine (\ds -> (map incPos ds, constC (map incPos ds) >>> c))
-}


game :: GameLogic
game 
  =  undefined -- constC () >>> aiDots initialSit >>> arr (\ds -> foldl (\a -> \b -> a ++ [mkRect b]) [] ds)


dotPos :: Velocity -> Pos -> Coroutine (Physics a, a) Dot
dotPos initialVel initialPos 
  = g >>> f >>> pos initialVel initialPos >>> arr (uncurry Dot)
  where
    g :: Coroutine (Physics a, a) (Coroutine a Acceleration,a)
    g = first $ arr $ looper (0,0) (+)
    f :: Coroutine (Coroutine a Acceleration, a) Acceleration
    f = arr (fst . uncurry runC)

aiDots :: [(Index, Physics (a, Dots))] -> Dots -> Coroutine a Dots
aiDots physics initialDots
  = loop $ g >>> withPrevious initialDots
  where
--    g :: Coroutine a Dots
--looper :: Dots -> (Dots -> Dots -> Dots) -> [Coroutine a Dots] -> Coroutine a Dots
    g = looper [] (++) $ map f physics
      where
--        f :: (Index, Physics a) -> Coroutine a Dots
        f (i, phys) 
          =     f' -- 
            >>> f'' --
            >>> f''' -- 
          where
            Just (Dot initialPos initialVel) = lookup i initialDots
--            f' :: Coroutine a (Index, (Physics a, a))
            f' = constC i &&& arr (phys,) 
--            f'' :: Coroutine (Index, (Physics a, a)) (Index, Dot)
            f'' =  second (dotPos initialVel initialPos) 
            f''' :: Coroutine (Index, Dot) Dots
            f''' = arr toDots

{- we want to have something that takes the input, splits it across all the acceleration vector calculators, then folds their output into an acceleration vector, then calculates their position
-}


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
dotCollision :: Coroutine (Index, Dots) Acceleration
dotCollision = collisions &&& returnA >>> arr collAccVecResolver

collisions :: Coroutine (Index, Dots) (Event Collision)
collisions = arr (\(i, ds) -> flip filter  [(i,x) | x <- [1..num], x /= i] (filterFunc ds))
  where
    filterFunc :: Dots -> (Int, Int) -> Bool
    filterFunc ds (d, d') = collides (al $ lookup d ds, al $ lookup d' ds)
       where
         collides :: (Dot, Dot) -> Bool
         collides ((Dot (x,y) _), (Dot (x',y') _))
           = ((dist (x'-x, y'-y)) - rad - rad') < delta
         Just (Dc { radius = rad }) = lookup d config
         Just  (Dc { radius = rad' }) = lookup d' config

-- should use looper here
collAccVecResolver :: (Event Collision, (Index, Dots)) -> Acceleration
collAccVecResolver ([],_)  = (0,0)
collAccVecResolver ((i,i'):cs, (index,ds)) = (xAcc'', yAcc'')
  where
    (xAcc'', yAcc'') = (xAcc + xAcc', yAcc + yAcc')
    (xAcc, yAcc)    = collAccVecResolver' (pos, vel) (pos', vel')
    (xAcc', yAcc')   = collAccVecResolver (cs, (index,ds))
    Just (Dot pos vel) = lookup i ds
    Just (Dot pos' vel') = lookup i' ds


collAccVecResolver' :: (Pos, Velocity) -> (Pos, Velocity) -> Acceleration
collAccVecResolver' ((x,y), _) ((x',y'), v')
  = project v' normal
  where
    normal = (x' - x, y' - y)

project (x,y) v = (x' * comp, y' * comp)
   where
     (x', y') = unit v
     comp     = x*x' + y*y'

unit (x,y) = (div x (dist (x, y) ), div y (dist (x, y)) )

dist (a,b) =  ceiling $ sqrt $ fromIntegral (a^2 + b^2)



--------------------------------------------------------------------------------
-- | Utilities


toDots :: (Index, Dot) -> Dots
toDots = (:[])

al (Just x) = x

allPairs :: [a] -> [(a,a)]
allPairs [] = []
allPairs (a:as) = map (a,) as ++ (allPairs as)

vecIntegrate (x,y) = integrate x *** integrate y

mkRect (i,Dot (x,y) _) = ((x-r,y-r),(2*r,2*r))
  where
    Just (Dc r) = lookup i config

instance Num a => Num (a, a) where
  (+) (a,a') (b, b') = (a+b, a'+b')
  (*) (a, a') (b,b') = (a*b, a'*b')
  abs v = undefined 
  signum (a, a') = undefined
  fromInteger i = undefined

--------------------------------------------------------------------------------

-- Test

play n = iterate (snd . f) game !! n

f = flip runC initKeyboard

g = fst . f . play


