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
infiniteLoop = fromList [(L 0, add 1 1), (L 1, sub 1 0 0)]

initialState :: State
initialState = (L 0, map (flip (,) 0.Reg) [1..])

add r j = Add (Reg r) (L j)
sub r j k = Sub (Reg r) (L j) (L k)


testFunc numR iters machine init = map (take numR.snd.snd) (take iters $ iterate (executeInstr infiniteLoop.snd) ((), initialState))

cTest1 = testFunc 1 5 infiniteLoop initialState == [[(Reg 1, 0)], [(Reg 1, 1)], [(Reg 1, 0)], [(Reg 1, 1)], [(Reg 1, 0)]]


testReg = [cTest1]



--------------------------------------------------------------------------------


    
{-
    if and tests
               then putStrLn "Tests Passed"
               else putStrLn "Tests Failed: Implement ErrorT Monad"
-}