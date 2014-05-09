module Test where

import Data.Map hiding (map)
import RegMachineEg
import MonadInstances

--------------------------------------------------------------------------------
-- TESTS Simple

sTest1 = run program1 0 0 == (False, 3)
sTest2 = run program1 0 1 == (True, 3)

testSimple = [sTest1, sTest2]


--------------------------------------------------------------------------------

-- TESTS Complex

infiniteLoop :: Machine
infiniteLoop = fromList [lb 0 (add 1 1), lb 1 (sub 1 0 0)]


zero :: Machine
zero = fromList [lb 0 (sub 1 0 1)]

-- add bindings for a register machine


initialState :: State
initialState = (L 0, map (flip (,) 0.Reg) [0..])

add r j = Add (Reg r) (L j)
sub r j k = Sub (Reg r) (L j) (L k)
lb = (,).L 




testFunc numR iters
  = (take numR.).(snd.).(snd.).runState.(runReaderT (foldl1 (>>) (replicate iters regReaderT)))

cTest1 = testFunc 1 5 infiniteLoop initialState == [(Reg 1, 1)]


testReg = [cTest1]



--------------------------------------------------------------------------------

tests = testReg ++ testSimple

runTestsuite = if and tests then putStrLn "Test Passed" else putStrLn "Test Failed"