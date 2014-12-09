module Dots.Controls
    ( up
    , down
    ) where

import Graphics.UI.GLUT (Key(..), SpecialKey(..))

up   = SpecialKey KeyUp
down = SpecialKey KeyDown
left = SpecialKey KeyLeft
right = SpecialKey KeyRight
