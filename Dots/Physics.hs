{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Dots.Physics where

import Prelude hiding (lookup)
import Data.Monoid
import Data.Map

import Control.Arrow
import Control.Coroutine

type Pos    = (Int, Int)
type Radius = Int
type DotPos = Pos
type Vector a = (a, a)
type Acceleration = Vector Int
type Velocity = Vector Int
type Index = Int
type Collision = (Index, Index)
data Dot a = Dot { radius :: Int, position :: DotPos, velocity :: Velocity, physics :: Physics a } deriving (Show)
type Physics a = [Coroutine (a, State a) Acceleration]
data State a = State { dots :: Map Index (Dot a) } deriving (Show)



instance Show (Coroutine a b) where
  show _ = " :Coroutine: "

instance Monoid (State a) where
  mempty = State{dots=mempty}
  mappend State{dots = l} State{dots = l'} 
     = State { dots = l `mappend` l' }



collides ::    Int 
            -> Map Index (Dot a) 
            -> (Index, Index) 
            -> ((DotPos, Velocity), (DotPos, Velocity)) 
            -> Bool
collides delta config (d, d') ((p@(x,y), v@(xV, yV)), (p'@(x',y'), v'@(xV', yV')))
   = inRange && headingToEachOther 
   where
     Just (Dot { radius = rad })  = lookup d config
     Just (Dot { radius = rad' }) = lookup d' config
     inRange = dist (x', y') (x, y) - rad - rad' < delta
     headingToEachOther = sameDirection
     sameDirection = dist p p' > dist (x+xV, y+yV) (x'+xV', y'+yV')


parallel (xV, yV) (xV', yV')
  = fromInteger xV / fromInteger yV == fromInteger xV' / fromInteger yV'

-- should use mconcat
collAccVecResolver :: ([Collision], (Index, Map Index (DotPos, Velocity))) -> Acceleration
collAccVecResolver ([],_)  = (0,0)
collAccVecResolver ((i,i'):cs, (index,ds)) = (xAcc'', yAcc'')
  where
    (xAcc'', yAcc'')  = (xAcc + xAcc', yAcc + yAcc')
    (xAcc, yAcc)      = collAccVecResolver' (pos, vel) (pos', vel')
    (xAcc', yAcc')    = collAccVecResolver (cs, (index,ds))
    Just (pos, vel)   = lookup i ds
    Just (pos', vel') = lookup i' ds


collAccVecResolver' ::    (Pos, Velocity) 
                       -> (Pos, Velocity) 
                       -> Acceleration
collAccVecResolver' ((x,y), _) ((x',y'), v')
  = project v' normal
  where	    
    normal = (x - x', y - y')

-- projects v onto v'
project :: (Int, Int) -> (Int, Int) -> (Int, Int)
project v v' = (truncate $ x' *  comp / distV' , truncate $ y' * comp / distV')
   where
     distV' = (x'^2+y'^2)
     (x, y) = (fromIntegral *** fromIntegral) v
     (x', y') = (fromIntegral *** fromIntegral) v'
     comp     =  abs $ x*x' + y*y'


unit (x,y) = ( x / (dist (x, y) (0,0)), y / (dist (x, y) (0,0)) )

dist (a,b) (a', b') = ceiling $ sqrt $ fromIntegral ((a-a')^2 + (b-b')^2)
