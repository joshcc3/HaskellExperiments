{-# LANGUAGE FlexibleInstances #-}

module RComp(Exp(..),Com(..), readerFunc, run, step) where

import MonadInstances
import Control.Monad
import Data.List

--------------------------------------------------------------------------------
-- EXPRESSION PRIMITIVES

--data Exp =   Var String  | Const Int  | Add Exp Exp deriving (Eq, Ord, Show)
data Exp =   Var String  | Const Int  deriving (Eq, Ord, Show)
type Result = Maybe Int
type Env = [(String , Int)]

always (Just x) = x

foldexp :: Exp -> Env -> Int
foldexp (Var var) env = always $ lookup var env
foldexp (Const n) env = n
--foldexp (Add e e') env = (foldexp e env) + (foldexp e' env)

-- LINK 7


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
-- COMMAND PRIMITIVES (Badly designed)


data Com =   Ass String (Rdr Env Result) | If Bool Com Com -- change Bool to BoolExp
           | Seq Com Com    | Skip | While Bool Com
-- LINK 3

-- LINK 4





run :: Com -> RComp Com Env -> Maybe Env
run program (R f) = case (f program) of
    	       	    (_, Left res) -> Just res
		    (env, Right s) -> Nothing


-- LINK 5
step :: Env -> RComp Com Env
step state = c 
  where
    c = R f
    f :: Com -> (Com, Either Env (RComp Com Env))
    f (Ass var exp) = let state' = update (var , runReader exp state) state
                      in (Skip, Left state')
    f (Skip)        = (Skip, Right c)
    f (If b c c')   = if b then (c, Left state)
                           else (c', Left state)
    f (Seq c c')    = let (c, res) = f c in (c', res)
    f (While b com) = f (If b (Seq com (While b com)) Skip)

    update :: (String, Result) -> Env -> Env
    update (_,Nothing) e  = e
    update (v, Just x) e  = (v,x):e

