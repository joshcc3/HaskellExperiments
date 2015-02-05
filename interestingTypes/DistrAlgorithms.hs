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




type Time = Int

type FPat = Time -> S.Set Bool

type FDet = FPat -> Hist

type ExitCode = Int

type Hist = forall a b. Proc a b -> Time -> FDetOut

data Proc'' a b m n = Unicast b Pid (m n) | Recieve (a -> m n) deriving (Functor)

type Proc' a b = Free (Proc'' a b IO)

type PMonSt m = ReaderT Config (StateT St m)

type Proc a b = PMonSt (Proc' a b) ExitCode

type FDetOut = PMonSt Identity (S.Set Int)

type Pid = Int

type Config = ([Pid], Size)

type St = Time

type Size = Int

type Delta = Time

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
      
