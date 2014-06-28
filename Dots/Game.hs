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
import qualified Data.Map as M
import Data.Monoid

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
config = M.fromList $ map (, Dot {radius = rad}) [1..num]
delta = 1


dot1 = Dot {radius = rad, position = (30,30), velocity = (6,6), physics = simplePhysics 1}

dot2 = Dot {radius = rad, position = (200,200), velocity = (2,2), physics = simplePhysics 2}

dot3 = Dot {radius = rad, position = (280,250), velocity = (1,1), physics = simplePhysics 3}

initialState :: State a
initialState = State (M.fromList [(1, dot1), (2, dot2), (3, dot3)])

simplePhysics :: Index -> Physics a
simplePhysics i = [dotCollAdapter i >>> dotCollision]

simplePhysicsList i = simplePhysics i : simplePhysicsList (i+1)

simplePhysicsSystem :: M.Map Index (Physics a)
simplePhysicsSystem = M.fromList $ zip [1..num] $ simplePhysicsList 1
--------------------------------------------------------------------------------
-- | Game logic  


{-
So we recieve the current state and parallelize the update of each component of the state - the dots, the animations etc. We then append all the changes together to build the new state. We then parallalize the transformation from the state components to rectangles.
-}
game :: GameLogic
game = loop $ g >>> h
  where
    g :: Coroutine (a, State a) (State a)
    g = parallelize co
    co :: [Coroutine (a, State a) (State a)]
    co = [aiDotPos simplePhysicsSystem initialDotPos >>> arr toState] 
    h :: Coroutine (State a) (Rects, State a)
    h = parallelize comps &&& delay initialState
    comps :: [Coroutine (State a) Rects]
    comps = [arr $ dotsToRect . dots ]
    initialDotPos = dots initialState

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
    g = first $ arr $ function
    f :: Coroutine (Coroutine a Acceleration, a) Acceleration
    f = arr (fst . uncurry runC)
    function :: [Coroutine (a, State a) Acceleration] -> Coroutine (a, State a) Acceleration
    function d = parallelize (map (>>> arr Sum) d) >>> arr getSum


aiDotPos :: forall a. M.Map Index (Physics a) 
                   -> M.Map Index (Dot a)
                   -> Coroutine (a, State a) 
                                (M.Map Index (Dot a))
aiDotPos physics initialDotPos 
  = parallelize $ 
       M.fold (:) [] $ 
          M.intersectionWithKey f physics initialDotPos
   where
    f i p dot@(Dot{position = pos, velocity = v})
      = physicsToCoroutine i p pos v >>> g dot
    g :: Dot a -> Coroutine (M.Map Index (DotPos, Velocity)) (M.Map Index (Dot a))
    g dot = arr $ M.map (\(p, v) -> dot{position = p, velocity = v})

-- interesting, this seems to act like a functor
physicsToCoroutine :: forall a. Index
                             -> Physics a
                             -> DotPos
                             -> Velocity
                             -> Coroutine (a, State a) 
                                          (M.Map Index (DotPos, Velocity))
physicsToCoroutine i phys initialPos initialVel 
  =     f' -- 
    >>> f'' --
    >>> f''' -- 
    where
      f' :: Coroutine (a, State a) (Index, (Physics a, (a, State a)))
      f'' :: Coroutine (Index, (Physics a, (a, State a))) 
                       (Index, (DotPos, Velocity))
      f''' :: Coroutine (Index, (DotPos, Velocity)) 
                        (M.Map Index (DotPos, Velocity))

      f' =  constC i &&& arr (phys,) 
      f'' =  returnA *** dotPos initialVel initialPos
      f''' = arr toDots




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

dotCollAdapter :: Index -> Coroutine (a, State a) (Index, M.Map Index (DotPos, Velocity))
dotCollAdapter i = constC i &&& (arr $ \(_,s) -> M.map (position &&& velocity) (dots s))  


dotCollision :: Coroutine (Index, M.Map Index (DotPos, Velocity)) Acceleration
dotCollision = collisions &&& returnA >>> arr  collAccVecResolver



collisions :: Coroutine (Index, M.Map Index (DotPos, Velocity)) 
                        (Event Collision)
collisions = arr $ f
  where
    filterFunc :: M.Map Index (DotPos, Velocity) -> (Int, Int) -> Bool
    filterFunc ds dots@(d, d') 
      = collides delta config dots (al $ M.lookup d ds, al $ M.lookup d' ds)
    f (i, ds) = flip filter pairs $ filterFunc ds
      where
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