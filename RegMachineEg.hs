import MonadInstances
import Data.Map

data Label  = L Int deriving (Eq, Ord, Show)
data Reg    = Reg Int deriving (Eq, Ord, Show)
type State  = (Label, Map Reg Int)
type Machine = Map Label Body
data Body = Add Reg Label | Sub Reg Label Label


simpleReader :: Rdr Int Bool
simpleReader = Reader (/= 0)


-- on :: (b -> b -> c) -> (a -> b) -> a -> a -> c
simpleState :: Bool -> St Int Bool
simpleState b  = State ((,) b.(+1))

bindF :: Bool -> StT Int (Rdr Int) Bool
bindF = StateT.(return.).runState.simpleState

program :: StT Int (Rdr Int) Bool
program
 = (liftST simpleReader) >>= bindF >>= bindF >>= bindF

run :: StT Int (Rdr Int) Bool -> Int -> Int -> (Bool, Int)
run  = (runReader.).runStateT
