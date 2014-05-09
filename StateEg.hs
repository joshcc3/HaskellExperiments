
import MonadInstances
import Control.Monad
import Data.List


-- LINK 10
pop :: St [a] a
pop = State (\(x:xs) -> (x, xs))

push :: a -> St [a] ()
push v = State (\l -> ((), v:l))

stackOperations 
  = do
    push 3
    push 4
    x <- pop
    y <- pop
    push 5
    push 3
    
