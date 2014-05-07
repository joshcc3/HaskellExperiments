module Test where

import RegMachineEg
import MonadInstances

--------------------------------------------------------------------------------
-- TESTS Simple

test1 = run program1 0 0 == (False, 3)
test2 = run program1 0 1 == (True, 3)

tests = [test1, test2]

runTestsuite = if and tests
               then putStrLn "Tests Passed"
               else putStrLn "Tests Failed: Implement ErrorT Monad"

--------------------------------------------------------------------------------

-- TESTS Complex

-- todo

--------------------------------------------------------------------------------
