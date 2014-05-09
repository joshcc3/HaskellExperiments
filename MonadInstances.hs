module MonadInstances where

import Control.Monad
import Data.List

-- LINK 1

-- LINK 2
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

data RComp r a = R (r -> (r, Either a (RComp r a)))


instance Monad (RComp r) where
--  return :: a -> RComp a
   return a = R (\c -> (c, Left a))

--   (>>=) :: RComp a -> (a -> RComp b) -> RComp b
   (>>=) (R rf) (f) = R (\c -> func (rf c) f)
     where
--func :: (Com, Either a (RComp a)) -> (a -> RComp b)->(Com, Either b (RComp b))
       func (c, Left v) f = let (R rf') = (f v) in rf' c
       func (c, Right suspended) f = (c, Right (suspended >>= f))


-- LINK 6

--------------------------------------------------------------------------------
