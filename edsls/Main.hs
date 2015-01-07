module Main where

import LambdaTerms
import LambdaSimplyT
import LambdaCalc
import Control.Monad.State

t :: LambdaT -> IO () 
t inp = do
  (out, _) <- flip runStateT "a" . ppC $ inp
  print . snd $ out



