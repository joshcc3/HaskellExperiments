{-# LANGUAGE DeriveFunctor, TupleSections #-}

import qualified Control.Category as C
--import qualified Control.Monad as M
import qualified MonadInstances as MI
import qualified Data.Map as M


data Free f r = Free (f (Free f r)) | Pure r

instance Functor f => Monad (Free f) where

   return = Pure

   (>>=) (Pure a) f    = f a
   (>>=) (Free f) func = Free (fmap (>>= func) f)


liftF :: Functor f => f r -> Free f r
liftF = Free . (fmap Pure)

data Unit = Unit { unitID :: Int, orientation :: Double, pos :: (Double, Double), stepLen :: Double } deriving (Show)


data UnitInstr next = Front Int StepLength next | Back Int StepLength next | RotateL Int Double next | RotateR Int Double next | Done

 
type StepLength = Double

type ID = Int

type GameState = M.Map ID Unit

type StatefulUnit a = MI.StT GameState (Free UnitInstr) a

instance Functor UnitInstr where
  fmap f (Front   u s n) = Front   u s (f n)
  fmap f (Back    u s n) = Back    u s (f n)
  fmap f (RotateL u d n) = RotateL u d (f n)
  fmap f (RotateR u d n) = RotateR u d (f n)




move (x, y) step orientation
 = (cos (orientation) * step * x, sin (orientation) * step * y)


step :: Free UnitInstr () -> GameState -> (Free UnitInstr (), GameState)
step (Pure ()) s = (Pure (), s)
step (Free (Front i step n)) s = (n, M.update f i s)
  where
    f (u@Unit{pos = initPos, stepLen = s, orientation = o}) = Just $u{pos = move initPos s o}
step (Free (Back i step n)) s = (n, M.update f i s)
  where
    f (u@Unit{pos = initPos, stepLen = s, orientation = o}) = Just $u{pos = move initPos (-s) o}
step (Free (RotateR i angle n)) s = (n, M.update f i s)
  where
    f (u@Unit{orientation = o}) = Just $u{orientation = angle + o}
step (Free (RotateL i angle n)) s = (n, M.update f i s)
  where
    f (u@Unit{orientation = o}) = Just $u{orientation = angle + o}

