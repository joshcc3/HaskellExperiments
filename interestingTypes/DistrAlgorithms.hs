{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
module Main where

import Control.Arrow
import qualified Control.Category as C
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Free
import Control.Lens
import qualified Data.Vector as V
import System.Random
import Test.QuickCheck
import Data.Maybe (fromMaybe)
import Control.Applicative
import Data.List
import Data.Char
import Data.Monoid
import qualified Data.Map as M
import qualified Data.Set as S
import Control.Monad
import Data.Functor.Foldable
import Control.Monad.Writer
import Control.Monad

type Time = Int

type FPat = Time -> S.Set Bool

type FDet = FPat -> Hist

type ExitCode = Int

type Hist = forall a b. Proc a b -> Time -> FDetOut

type FDetOut = PMonSt Identity (S.Set Int)

type Pid = Int

type Config = ([Pid], Size)

type St = ()

type Size = Int

type Delta = Time

type Proc' a b = Free (Proc'' a b)

type PMonSt m = ReaderT Config (StateT St m)

type Proc a b = PMonSt (Proc' a b) ExitCode

data Proc'' a b n = Wait Int | Unicast b Pid n | 
                    QSize (Int -> n) | Recieve ((a, Pid) -> n) deriving (Functor)

data TimerReq = CurrentTime

data Output = WaitO Pid Int | UnicastO Pid String Pid 

timerService1 :: Time -> Proc' TimerReq Int ExitCode
timerService1 t = do
  liftF $ Wait 1
  Free $ QSize f
      where
        f x | x == 0 = timerService1 (t+1)
            | otherwise = do
                  forM_ [1..x] $ \_->
                        Free $ Recieve g
                  timerService1 (t+1)

        g (CurrentTime, pid) = Free $ Unicast (t+1) pid (timerService1 (t+1))

toMSt = lift . lift . liftF

{-
proc1 = do
  config <- ask
  toMSt $ Wait 10 
  Unicast CurrentTime
-}

interp :: Proc' TimerReq Int ExitCode -> IO Int
interp = undefined 


{-
  The question is, what is the abstraction that we offer to the user.
  We are going to implement the actor model of concurrency.
  In the actor model, the abstraction that we provide to a user is,
  being able to send messages to some other process.
  A message queue
  A uniform notion of rate of change of time with time.

  The interface that a message queue offers is:
    being able to check how many outstanding messages there are
    sleeping till a message arrives

-}

{-
  Now using these abstractions its time to start creating some services
  The first service that we're going to create os a timer service.
  The service must expose the following interface
  you should be able to query the time
  you should be able to set a reminder such that at the specific time
  you send the reminder to the individual

We also make the assumption that the of computatino is negligibel
compare to the time that we wait for.
We wait the required time and check if there are any requests for the
time

-}



{-


stronglyAccurate1 :: FDet
stronglyAccurate1 _ _ _ = return S.empty

stronglyComplete1 :: FDet
stronglyComplete1 _ _ _ = do
  (pids, _) <- ask
  return (S.fromList pids)


unicast :: b -> Pid -> Proc a b
unicast b pid = lift . liftF $ Unicast b pid (return 0)

hbBLoop :: Time -> Pid -> Delta -> Proc a ()
hbBLoop startTime pid delta = do
  time <- get
  if time - startTime `mod` delta == 0
  then unicast () pid
  else hbBLoop startTime pid delta
-}
