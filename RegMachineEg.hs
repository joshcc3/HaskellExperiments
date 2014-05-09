module RegMachineEg where

import MonadInstances
import Data.Map as M hiding (update, delete)
import Data.List (delete)

data Label  = L Int deriving (Eq, Ord, Show)
data Reg    = Reg Int deriving (Eq, Ord, Show)
data Body = Add Reg Label | Sub Reg Label Label
type State  = [(Reg,Int)]
type Machine = Map Label Body
type RegisterBindings = Map Int Int
type RegisterMachine = RdrT Machine (St State) Label


relabel :: RegisterBindings -> Machine -> Machine
relabel rb m = undefined

gadgetReaderT :: RdrT RegisterBindings (RdrT Machine (St State)) Label
gadgetReaderT = undefined
--p = ReaderT $ ((ReaderT).(runReaderT regReaderT.)).relabel


start = return (L 0) :: RegisterMachine

step :: Label -> RegisterMachine
step = ReaderT.((State.).executeInstr)

runReg = (runState.).runReaderT 




update :: (Int -> Int) -> Reg -> [(Reg, Int)] -> [(Reg, Int)]
update f r regs = (r ,f value):(delete (r, value) regs)
  where
    (Just value) = Prelude.lookup r regs



executeInstr ::  Label -> Machine -> State -> (Label, State)
executeInstr l config regs = func (M.lookup l config)
  where
    func :: Maybe Body -> (Label, State)
    func Nothing  = (L (-1), regs)
    func (Just (Add reg label)) = (label, update (+1) reg regs)
    func (Just (Sub reg l l')) = if x == 0 then (l, regs)
                                           else (l, update ((-)1) reg regs)
      where
       (Just x) = Prelude.lookup reg regs 




-------------------------------------------------------------------------------- State Monad Transformer example

-- simpleReader :: Rdr Int Bool
simpleReader = Reader (/= 0)

-- on :: (b -> b -> c) -> (a -> b) -> a -> a -> c
simpleState :: Bool -> St Int Bool
simpleState b  = State ((,) b.(+1))

bindF :: Bool -> StT Int (Rdr Int) Bool
bindF = StateT.(return.).runState.simpleState

program1 :: StT Int (Rdr Int) Bool
program1
 = (liftST simpleReader) >>= bindF >>= bindF >>= bindF

runSimple :: StT Int (Rdr Int) Bool -> Int -> Int -> (Bool, Int)
runSimple  = (runReader.).runStateT
