{-# LANGUAGE DeriveFunctor, TupleSections #-}

import qualified Control.Category as C
--import qualified Control.Monad as M
import qualified MonadInstances as MI
import qualified Data.Map as M
import Data.Monoid

data Free f r = Free (f (Free f r)) | Pure r

instance Functor f => Monad (Free f) where

   return = Pure

   (>>=) (Pure a) f    = f a
   (>>=) (Free f) func = Free (fmap (>>= func) f)


liftF :: Functor f => f r -> Free f r
liftF = Free . (fmap Pure)

data Unit = Unit { unitID :: Int, orientation :: Double, pos :: (Double, Double), stepLen :: Double } deriving (Show)


data UnitInstr next = Front Int StepLength next | Back Int StepLength next | RotateL Int Double next | RotateR Int Double next | Done

data LogEntry a = Log { time :: Int, instr :: Free UnitInstr a }
 
type StepLength = Double

type ID = Int

type GameState = M.Map ID Unit

type StatefulUnit a = MI.St GameState (Free UnitInstr a)

type LoggedUnit b a   = MI.WriterT [LogEntry b] (MI.St GameState) (Free UnitInstr a)


instance Functor UnitInstr where
  fmap f (Front   u s n) = Front   u s (f n)
  fmap f (Back    u s n) = Back    u s (f n)
  fmap f (RotateL u d n) = RotateL u d (f n)
  fmap f (RotateR u d n) = RotateR u d (f n)
  fmap f Done            = Done



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
-- use a writer to put note changes in the game and serialize those changes


stepWriter :: Int -> Free UnitInstr () -> LoggedUnit () ()
stepWriter t (Pure ()) = return $ return ()
stepWriter t stat@(Free (Front i s n)) 
   = MI.liftWT [Log {time = t, instr = liftF (Front i s ()) }] (MI.State (step stat))
stepWriter t stat@(Free (Back i s n)) 
   = MI.liftWT [Log {time = t, instr = liftF (Back i s ()) }] (MI.State (step stat))
stepWriter t stat@(Free (RotateR i angle n)) 
   = MI.liftWT [Log {time = t, instr = liftF (RotateR i angle ()) }] (MI.State (step stat))
stepWriter t stat@(Free (RotateL i angle n)) 
   = MI.liftWT [Log {time = t, instr = liftF (RotateL i angle ()) }] (MI.State (step stat))

{-
   so what we want is a writer that transforms the state monad and
   adds to a log of all the instructions we execute. 
-}