{-# LANGUAGE Arrows, TupleSections, ScopedTypeVariables #-}

module Dots.Game where

import Control.Arrow
import Control.Coroutine
import Control.Coroutine.FRP



import Dots.Keyboard
import Dots.Controls
import Dots.Rect hiding (Pos)
import Dots.Physics
import Dots.Utils

import Data.List
import Data.Bifunctor (bimap)

{- 
We want to make a simple game where there are some dots (circles). Each of these dots move in a fixed path. When the dots collide they bounce off according to the rules of physics. In addition the user manually controls one dot. Up scales the speed vector up, back scales the speed vector down until it reflects it across the axis. Left changes the speed vector by a constant angle in the anti clockwise direction. Right changes the speed vector by a constant angle in the clockwise direction. The bouncing of the dots is effected by the reflection of the speed vector against the tangent to the point of contact between the two dots. 
-}

{-
The position of the dot is the integration of the speed vector. The speed vector is the integration of the acceleration vector. The acceleration vector for all except the users bot is constant except at collisions. And collisions depend on the units positions, thus we have an arrow loop.
-}

type GameLogic = Coroutine Keyboard Rects

--------------------------------------------------------------------------------
-- | Game Initialization
num = 3
rad = 20
config = map (, Dot {radius = rad}) [1..num]
delta = 2

initialState :: State a
initialState = State [(1, Dot {radius = rad, position = (10,10), velocity = (10,10), physics = simplePhysics} )]

simplePhysics :: Physics a
simplePhysics = [dotCollAdapter >>> dotCollision]

--------------------------------------------------------------------------------
-- | Game logic  


{-
So we recieve the current state and parallelize the update of each component of the state - the dots, the animations etc. We then append all the changes together to build the new state. We then parallalize the transformation from the state components to rectangles.
-}
game :: GameLogic
game = loop $ g >>> h
  where
    g :: Coroutine (a, State a) (State a)
    g = parallelize mempty mappend co
    co = [aiDotPos simplePhysics initialSit >>> arr toState] 
    h :: Coroutine (State a) (Rects, State a)
    h = parallelize [] (++) comps &&& delay initialState
    comps :: [Coroutine (State a) Rects]
    comps = [arr $ dotsToRect . dots ]


{-
The dot position is a transformation from the current state to a new position and velocity. 
-}
dotPos ::    Velocity 
          -> Pos 
          -> Coroutine (Physics a, (a, State a)) 
                       (DotPos, Velocity)

dotPos initialVel initialPos 
  = g >>> f >>> pos initialVel initialPos
  where
    g :: Coroutine (Physics a, (a, State a)) 
                    (Coroutine (a, State a) Acceleration, (a, State a))
    g = first $ arr $ parallelize (0,0) (+)
    f :: Coroutine (Coroutine a Acceleration, a) Acceleration
    f = arr (fst . uncurry runC)



aiDotPos :: forall a. Map Index (Physics a) 
                   -> Map Index (DotPos, Velocity) 
                   -> Coroutine (a, State a) 
                                (Map Index (DotPos, Velocity))
aiDotPos physics initialDotPos 
  = parallelize [] (++) (map (physicsToCoroutine initialDotPos) physics)


-- interesting, this seems to act like a functor
physicsToCoroutine :: forall a. Map Index (DotPos, Velocity) 
                             -> (Index, Physics a) 
                             -> Coroutine (a, State a) 
                                          (Map Index (DotPos, Velocity))
physicsToCoroutine initialDots (i, phys) 
  =     f' -- 
    >>> f'' --
    >>> f''' -- 
    where
      f' :: Coroutine (a, State a) (Index, (Physics a, (a, State a)))
      f'' :: Coroutine (Index, (Physics a, (a, State a))) 
                       (Index, (DotPos, Velocity))
      f''' :: Coroutine (Index, (DotPos, Velocity)) 
                        (Map Index (DotPos, Velocity))

      f' =  constC i &&& arr (phys,) 
      f'' =  returnA *** dotPos initialVel initialPos
      f''' = arr toDots
      Just (initialPos, initialVel) = (lookup i initialDots)



{-
we want to have something that takes the input, splits it across all the acceleration vector calculators, then folds their output into an acceleration vector, then calculates their position
-}


pos ::    Velocity 
          ->  Pos 
          ->  Coroutine (Acceleration) (Pos, Velocity)
pos initialVel initialPos 
   =     vecIntegrate initialVel 
     >>> vecIntegrate initialPos &&& arr id

--------------------------------------------------------------------------------
-- | Physics Coroutines

collisionList = collisions:collisionList

dotCollAdapter :: Coroutine a (Index, Map Index (DotPos, Velocity))
dotCollAdapter = undefined


dotCollision :: Coroutine (Index, Map Index (DotPos, Velocity)) Acceleration
dotCollision = collisions &&& returnA >>> arr collAccVecResolver



collisions :: Coroutine (Index, Map Index (DotPos, Velocity)) 
                        (Event Collision)
collisions = arr $ \(i, ds) -> flip filter pairs $ filterFunc ds
  where
    filterFunc :: Map Index (DotPos, Velocity) -> (Int, Int) -> Bool
    filterFunc ds dots@(d, d') 
      = collides delta config dots (al $ lookup d ds, al $ lookup d' ds)
    pairs = [(i,x) | x <- [1..num], x /= i]


--------------------------------------------------------------------------------
-- | Utilities



--------------------------------------------------------------------------------

-- | Test

play n = iterate (snd . f) game !! n

f = flip runC initKeyboard

g = fst . f . play


{-
game' :: GameLogic
game' =      constC initialSit 
         >>> c 
         >>> (arr $ foldl (\a -> \b -> a ++ [mkRect b]) [])
  where
    incPos (i, Dot (x,y) v) = (i, Dot(x+1, y+1) v)
    c = Coroutine $ \ds -> (map incPos ds, constC (map incPos ds) >>> c)
-}