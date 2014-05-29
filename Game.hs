{-# LANGUAGE DeriveFunctor, TupleSections #-}

import Control.Monad.State
import Thread
import qualified Data.Map as M

data Unit = Unit { orientation :: Int, pos :: (Double, Double), stepLen :: Double } deriving (Show)


type StepLength = Double

type ID = Int

type GameState = M.Map ID Unit


toRad :: Int -> Double
toRad d = (fromIntegral d) * pi/180

newPos (x, y) step orientation
 = (cos (toRad orientation) * step + x, sin (toRad orientation) * step + y)

newAngle initial rotation = (360 + initial + rotation) `mod` 360

mov :: Int -> StepLength -> State GameState ()
mov i step
 = do
   s <- get
   put (M.update updFunc i s)
   return ()
   where
     updFunc :: Unit -> Maybe Unit
     updFunc u@(Unit {pos = p, orientation = o}) = Just $ u{pos = newPos p step o} 

rot :: Int -> Int -> State GameState ()
rot i angle 
  = do
    s <- get
    put (M.update updFunc i s)
    return ()
    where
      updFunc :: Unit -> Maybe Unit
      updFunc u@(Unit {orientation = o}) = Just $ u { orientation = newAngle o angle }


strat1
 = replicateM_ 12 $ do
    replicateM_ 5 (mov 1 10)
    rot 1 180




  