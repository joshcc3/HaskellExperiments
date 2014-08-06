{-# LANGUAGE FlexibleInstances #-}

import Control.Monad.Free
import Control.Arrow
import Control.Applicative
import qualified Data.Map as M
import Data.Char

type VarName       = Char

type Value         = Integer

type Exp           = Value

type St            = M.Map VarName Value

newtype Reader r a = Reader { runReader :: r -> a }

newtype State s a  = State { runState :: s -> (a, s) }

type ComB          = Reader St Bool
    
type ComE          = Reader St Exp

data Com           = Ass VarName ComE | Skip

data CFlow a       = If ComB a a | While ComB a | Seq a a

type Coms          = Free CFlow Com
 

smallStep :: Coms -> State St Coms
smallStep (Free (If b t e)) 
    = do
  s <- get
  if (runReader b s) then return t else return e

smallStep w@(Free (While b c)) 
    = return (Free (If b (Free $ Seq c w) (Pure Skip)))
smallStep (Pure (Ass var exp)) 
    =  do	  
  s <- get
  let v = runReader exp s
  put $ M.update (const (Just v)) var s
  return $ Pure $ Skip 
smallStep (Pure Skip) = return $ Pure Skip


fact :: Integer -> Free CFlow Com
fact x
 = do
    'r' ~= num 1
    'i' ~= num x
    whileC (var 'i' ~> num 0) $ 
      'r' ~= (var 'r' * var 'i')
 
    
  

instance Functor CFlow where 
  fmap f (If b t e)    = If b (f t) (f e)
  fmap f w@(While b c) = While b (f c)
  fmap f (Seq c c')    = Seq (f c) (f c')

{-
instance (Functor f) => Monad (Free f) where
    return = Pure
    (Free x) >>= f = Free (fmap (>>= f) x)
    (Pure r) >>= f = f r
-}
instance Functor (Reader r) where
  fmap f (Reader rf) = Reader (f . rf)


instance Monad (Reader r) where
  return a  = Reader $ const a
  (>>=) r f = Reader $ \s -> runReader (f (runReader r s)) s

instance Applicative (Reader r) where
 pure a                     = Reader $ const a
 (Reader rf) <*> (Reader r) = Reader $ \s -> rf s (r s) 


instance Functor (State s) where
  fmap f sF = State func
    where
      func s = (f a, s')
        where
          (a, s') = runState sF s 


instance Monad (State s) where
  return a = State $ \s -> (a,s)
  (>>=) s f = State $ uncurry runState . (f *** id) . runState s
       

put :: s -> State s ()
put s = State $ const ((),s)

get :: State s s
get = State $ \s -> (s, s)

instance Num (Reader St Exp) where
 (+)         = liftA2 (+)
 (*)         = liftA2 (*)
 abs         = liftA abs
 signum      = liftA signum
 fromInteger = pure


-- | Utilities
(~=) i v = Pure $ Ass i v
(~>)  :: Ord o => Reader r o -> Reader r o -> Reader r Bool
(~>)     = liftA2 (>)
(~<)  :: Ord o => Reader r o -> Reader r o -> Reader r Bool
(~<)     = liftA2 (<)
(~==) :: Eq e => Reader r e -> Reader r e -> Reader r Bool
(~==)    = liftA2 (==)
(~&&) :: Reader r Bool -> Reader r Bool -> Reader r Bool
(~&&)    = liftA2 (&&)
(~||) :: Reader r Bool -> Reader r Bool -> Reader r Bool
(~||)    = liftA2 (||)


al (Just x) = x

var   :: Char -> Reader St Exp
var = Reader . (al .). M.lookup

num   :: Integer -> Reader St Exp
num x = pure x

exp1  :: Reader St Exp
exp1 = var 'a' + var 'b' - num 3

whileC :: ComB -> Free CFlow a -> Free CFlow a
whileC = (Free .). While


ifC
  :: ComB
     -> CFlow (Free CFlow a) -> CFlow (Free CFlow a) -> Free CFlow a
ifC b t e = Free $ If b (Free t) (Free e)


initState :: M.Map Char Integer
initState = M.fromList [(chr x, 0) | x <- [97..123]]

-- test n p = foldl ($) replicate n (>>= smallStep)