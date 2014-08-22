module Main where

import Prelude hiding (Either(..), either, (*))
import Machine
import RegEx

main = do
    s <- getLine
    print $ view $ run ((*) $ ifR <.> spc <.> forR) s
