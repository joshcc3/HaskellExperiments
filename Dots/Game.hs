{-# LANGUAGE Arrows, TupleSections, FlexibleInstances, ScopedTypeVariables #-}

module Dots.Game where

import Control.Arrow
import Control.Coroutine
import Control.Coroutine.FRP

import Data.Bifunctor (bimap)

import Dots.Keyboard
import Dots.Controls
import Dots.Rect hiding (Pos)

import Data.List

{-
Right so what we want to do is create a game. All components of the game are co-routines. That is, all elements of the games are modelled as time varying values.
A dot is a co-routine from Events, to a shape. 
-}

type X = Int
type Y = Int
type Coords = (X, Y)
type Shape = [Coords]

type Dot = Coroutine Event Shape
type 
