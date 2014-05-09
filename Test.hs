module Test where

import RComp
import MonadInstances

--------------------------------------------------------------------------------
-- TESTS EXP

num x = Reader $ readerFunc (Const x)
var x =  Reader $ readerFunc (Var x)

ex1 = (num 5) + (num 10) + (var "x") * (num 10)

ex2 = (num 10) + (var "x")


test1 = runReader ex1 [] == Nothing
test2 = runReader ex1 [("x",5)] == ( Just 65)

tests = [test1, test2]

runTestsuite = if and tests 
	       then putStrLn "Tests Passed"
	       else putStrLn "Tests Failed: Implement ErrorT Monad"

--------------------------------------------------------------------------------

-- TESTS COM

program :: Com
program = (Seq (Ass "x" (num 0)) (Ass "x" ((var "x") + 2)))



--------------------------------------------------------------------------------
