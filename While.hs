{-# LANGUAGE FlexibleInstances #-}

import Control.Monad
import Data.List

{- 
   Constructing the state monad. The state monad is simply a monad
   that holds a state function. A state function is a function that
   takes a state and generates a value and a new state out of the 
   state. 
   The state monad must evidently be a functor. A functor is something
   that relates categories. It relates the Hask category with the 
   St category a sub category of Hask. It is the category that is 
   parameterized by a state. It maps tyeps from Hask to types in State.
   It maps morphisms in Hask, to morphisms in State. That is it provides
   a computational context for values.
-}

-- the state constructor State :: (s -> (a,s)) -> St s a
-- that is it takes a function that takes a state and produces
-- a value and an updated state.
data St s a = State {runState :: s -> (a, s) }

instance Functor (St s) where
--fmap :: (a -> b) -> St s a -> St s b
  fmap f (State sf) = State (\state -> let (v, newState) = sf state 
                                       in (f v, newState))

instance Monad (St s) where

  return a = State (\state -> (a, state))
  
  (>>=) (State sf) g = State (\state -> let (v, newState) = sf state 
                                        in runState (g v) newState)

readSt = State (\s -> (s, s))

updateSt f = State (\s -> ((), f s))

--------------------------------------------------------------------------------



data Rdr r a = Reader{ runReader :: r -> a } 

instance Monad (Rdr r) where
 return x = Reader (\r -> x)
--(>>=) Rdr r -> (a -> Rdr b) -> Rdr b
 (>>=) reader fReader 
   = Reader (\r -> runReader (fReader (runReader reader r)) r)

getEnv :: r -> Rdr e r
getEnv r = Reader (\_ -> r)

--------------------------------------------------------------------------------


newtype MbT m a = MaybeT { runMaybeT :: m (Maybe a) }

instance Monad m => Monad (MbT m) where

  return x = MaybeT $ return (Just x)

  (>>=) (MaybeT innermonad) f = MaybeT ((>>=) innermonad (func f) )
    where
      func f Nothing  = return Nothing
      func f (Just x) = runMaybeT (f x)

--------------------------------------------------------------------------------

--data Exp =   Var String  | Const Int  | Add Exp Exp deriving (Eq, Ord, Show)
data Exp =   Var String  | Const Int  deriving (Eq, Ord, Show)
type Result = Maybe Int
type Env = [(String , Int)]


--------------------------------------------------------------------------------

data Com =   Ass String (Rdr Env Result) | If Bool Com Com -- change Bool to BoolExp
           | Seq Com Com    | Skip | While Bool Com
-- create a type class for types that define well founded orderings on instances
{- 
   in our case of computing resources, the resource is a sequence of 
   commands. The value produced by the sequence of commands is a state function
   or a binding of variables to values the type Env
-}

-- the step function performs a (small step?) operation on the commands
step :: Env -> RComp Env
step state = c 
  where
    c = R f
    f :: Com -> (Com, Either Env (RComp Env))
    f (Ass var exp) = let state' = update (var , runReader exp state) state
                      in (Skip, Left state')
    update :: (String, Result) -> Env -> Env
    update (_,Nothing) e  = e
    update (v, Just x) e  = (v,x):e


--------------------------------------------------------------------------------

data RComp a = R (Com -> (Com, Either a (RComp a)))


instance Monad RComp where
--  return :: a -> RComp a
   return a = R (\c -> (c, Left a))

--   (>>=) :: RComp a -> (a -> RComp b) -> RComp b
   (>>=) (R rf) (f) = R (\c -> func (rf c) f)
     where
--func :: (Com, Either a (RComp a)) -> (a -> RComp b)->(Com, Either b (RComp b))
       func (c, Left v) f = let (R rf') = (f v) in rf' c
       func (c, Right suspended) f = (c, Right (suspended >>= f))

{- What happens in the bind function is that the initial context for resource 
 utilization if by applying the resource supplied to the original context
 results in a value, we simply feed this value to the function that takes
 takes values and generates resource contexts out of them, that is the
 resource context depends on the value of another computation on a resource
 much like in function calls. 
 If the result of the first operation is a suspended computation because the 
 resources ran out, then we just return the result - the resource we have
 uptil now and the suspended computation is now the original suspended computation
 bound into the subsequent computation denoted by f
-}

--------------------------------------------------------------------------------






instance Num (Rdr Env Result) where
  (+) = liftM2 (liftM2 (+))
  (*) = liftM2 (liftM2 (*))
  abs  = liftM (liftM abs) 
  signum = liftM (liftM signum)   
  fromInteger i = Reader (\_ -> (Just . fromInteger) i)


readerFunc :: Exp -> Env -> Result
readerFunc (Const n) env  = return (n :: Int) 
readerFunc (Var var) env  = lookup var env
--readerFunc (Add e e') env = liftM2 (+) (readerFunc e env) (readerFunc e' env)

--------------------------------------------------------------------------------


always (Just x) = x

foldexp :: Exp -> Env -> Int
foldexp (Var var) env = always $ lookup var env
foldexp (Const n) env = n
--foldexp (Add e e') env = (foldexp e env) + (foldexp e' env)




--------------------------------------------------------------------------------
-- TESTS

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


{- Working
   (+) :: (MbT (Rdr Env) Exp) -> (MbT (Rdr Env) Exp) -> (MbT (Rdr Env) Exp)

   abs :: (MbT (Rdr Env) Exp) -> (MbT (Rdr Env) Exp)
   abs (MaybeT readerM) = MaybeT (Reader (\env -> lift (abs) (runReader readerM env)))
-}
 






{-
eval :: Exp -> Env -> (Maybe Int, Env)
eval (Var var) env 
 = (lookup var env, env)
 = (,) (lookup var env) env

let (lookup var) = g
    (,)          = h
    
eval env = h (g env) env
eval :: (Int -> Env -> (Int, Env)) -> (Env -> Int) -> (Int, Env)

Thus in this case the reader monad is environment

eval h g env = h (g env) env

instance Monad (-> r) where
(>>=) :: m a -> (a -> m b) -> m b
(>>=) :: (r -> a) -> (a -> r -> b) -> r -> b

f :: (a -> b) ->  (b -> a -> c) -> a -> c
a = r, b = a, c = b
f :: (r -> b) -> (a -> r -> b) -> r -> b

f y = h (g y) y = (>>=) g h


 = (>>=) (lookup var) (,) env

eval (Var var) = (>>=) (lookup var) (,)

Thus eval is just bind with a specific function for h and g
thus

-}



