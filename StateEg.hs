
import MonadInstances
import Control.Monad
import Data.List


-- the state monad takes a function that takes a state
-- and returns a pair of the updated value and the new state


pop :: [a] -> (a, [a])
pop l = (,) (head l) (tail l)

push :: a -> [a] -> ((), [a])
push x l = ((), x:l)

popContext :: St [a] a
popContext = State pop

pushContext :: a -> St [a] ()
pushContext = State . push


stackOperations 
 = do 
   pushContext 3
   pushContext 5
   pushContext 6
   pushContext 7
   x <- popContext
   y <- popContext
   pushContext x
   pushContext y
