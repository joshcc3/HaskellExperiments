module Test where

import qualified Data.Map as M

import Control.Coroutine
import Control.Arrow

import Dots.Game
import Dots.Physics
import Dots.Utils

prettifier :: Dot a -> String
prettifier (Dot{position = p, velocity = v})
  = "(Position = " ++ (show p) ++ "; " ++ "Velocity = " ++ (show v) ++ ") "

debug :: State a -> State a
debug s = s'
  where
    (s', _) = runC (aiDotPos simplePhysicsSystem (dots s) >>> arr toState) (undefined, s)
    

coll = [(1,2)]
dotP1 = (30,30)
dotV1 = (1,1)
d1 = (dotP1, dotV1)


dotP2 = (40,40)
dotV2 = (-1,-1)
d2  = (dotP2, dotV2)


collAccVecResolver' ::    (Pos, Velocity) 
                       -> (Pos, Velocity) 
                       -> Acceleration
collAccVecResolver' ((x,y), _) ((x',y'), v')
  = project v' normal
  where	    
    normal = (x' - x, y' - y)

-- M.fromList [(1, ((30,30),(6,6))), (2, ((40,40),(2,2)))]
-- (2,2) (10,10)
-- 

--v = (-1, -1)
--v' = (10, 10)
{-project :: (Int, Int) -> (Int, Int) -> (Int, Int)
project v v' = (round $ x'* comp /distV' , round $ y' * comp / distV')
   where
     distV' = (x'^2+y'^2)
     (x, y) = (fromIntegral *** fromIntegral) v
     (x', y') = (fromIntegral *** fromIntegral) v'
     comp     = x*x' + y*y'
-}


colls = (coll, (1, M.fromList [(1, d1), (2, d2)]))

deb = collAccVecResolver colls