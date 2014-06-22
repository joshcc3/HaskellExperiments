module Dots.Physics where

import Control.Coroutine

type Pos    = (Int, Int)
type Radius = Int
type DotPos = Pos
type Vector a = (a, a)
type Acceleration = Vector Int
type Velocity = Vector Int
type Index = Int
type Collision = (Index, Index)
data Dot a = Dot { radius :: Int, position :: DotPos, velocity :: Velocity, physics :: Physics a }
type Physics a = [Coroutine (a, State a) Acceleration]
data State a = State { dots :: Map Index (Dot a) }
type Map a b = [(a, b)]



dotsToRect :: Map Index (Dot a) -> Rects
dotsToRect m =  foldWithKey func [] m
  where
    func b k a = mkRect k a : b


instance Monoid (State a) where
  mempty = State{dots=[]}
  mappend State{dots = l} State{dots = l'} 
     = State { dots = l `mappend` l' }

instance Monoid b => Monoid (Map a b) where
  mempty = []
  mappend l l' = unionWith mappend [l, l']


collides ::    Int 
            -> Map Index (Dot a) 
            -> (Index, Index) 
            -> ((DotPos, Velocity), (DotPos, Velocity)) 
            -> Bool
collides delta config (d, d') (((x,y), _), ((x',y'), _))
   = ((dist (x'-x, y'-y)) - rad - rad') < delta
   where
     Just (Dot { radius = rad })  = lookup d config
     Just (Dot { radius = rad' }) = lookup d' config

-- should use looper here
collAccVecResolver :: ([Collision], (Index, Map Index (DotPos, Velocity))) -> Acceleration
collAccVecResolver ([],_)  = (0,0)
collAccVecResolver ((i,i'):cs, (index,ds)) = (xAcc'', yAcc'')
  where
    (xAcc'', yAcc'') = (xAcc + xAcc', yAcc + yAcc')
    (xAcc, yAcc)    = collAccVecResolver' (pos, vel) (pos', vel')
    (xAcc', yAcc')   = collAccVecResolver (cs, (index,ds))
    Just (pos, vel) = lookup i ds
    Just (pos', vel') = lookup i' ds


collAccVecResolver' ::    (Pos, Velocity) 
                       -> (Pos, Velocity) 
                       -> Acceleration
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


