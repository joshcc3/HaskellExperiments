module MonadInstances where

--------------------------------------------------------------------------------

{-
   MONAD LAWS:

   return a >>= k  =  k a
   m >>= return    =  m
   m >>= (\x -> k x >>= h)  =  (m >>= k) >>= h
 
   fmap f xs  =  xs >>= return . f  =  liftM f xs

-}


--------------------------------------------------------------------------------

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

instance Functor (Rdr r) where
  fmap f (Reader rf) = Reader (f.rf)

instance Monad (Rdr r) where
 return x = Reader (\r -> x)
--(>>=) Rdr r -> (a -> Rdr b) -> Rdr b
 (>>=) reader fReader
   = Reader (\r -> runReader (fReader (runReader reader r)) r)

getEnv :: r -> Rdr e r
getEnv r = Reader (\_ -> r)

--------------------------------------------------------------------------------

data RdrT r m a = ReaderT { runReaderT :: r -> m a}

instance Monad m => Monad (RdrT r m) where
  return = ReaderT .const.return
  
  (>>=) (ReaderT rf) f = ReaderT (\r -> (rf r) >>= (flip runReaderT r.f))

--------------------------------------------------------------------------------


newtype MbT m a = MaybeT { runMaybeT :: m (Maybe a) }

instance Monad m => Monad (MbT m) where

  return x = MaybeT $ return (Just x)

  (>>=) (MaybeT innermonad) f = MaybeT ((>>=) innermonad (func f) )
    where
      func f Nothing  = return Nothing
      func f (Just x) = runMaybeT (f x)

--------------------------------------------------------------------------------
-- LINK 11

newtype StT s m a = StateT { runStateT :: s -> m (a, s) }

instance (Monad m) => Monad (StT s m) where
  return x = StateT (\s -> return (x, s))

-- (>>=) :: Monad m => m a -> (a -> m b) -> m b
--  (>>=) :: StT s m a -> (a -> StT s m b) -> StT s m
  (>>=) (StateT innermonad) f
    = StateT (\s -> (innermonad s) >>= f')
     where
       f' (a,s) = (runStateT (f a)) s

liftST :: Rdr r a -> StT s (Rdr r) a
liftST reader = StateT (\s -> Reader (\r -> (runReader reader r, s)))


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

-- Cont Monad :D

newtype Ct r a = Cont {runCont :: (a -> r) -> r}

instance Monad (Ct r) where
  return arg = Cont ($arg)

-- LINK 12
--  (>>=) :: Cont r a -> ( a -> Cont r b) -> Cont r b
-- g :: (a -> r) -> r
-- f :: a -> (b -> r) -> r
-- x :: b -> r
-- g -> f -> ((b -> r) -> r)
-- a -> Cont ((b->r) -> r)
  (>>=) g f = Cont (\c -> runCont g (\a -> runCont (f a) c))


callCC :: ((a -> Ct r b) -> Ct r a) -> Ct r a
callCC arg = Cont func
  where
--  func :: (a -> r) -> r
    func f = runCont (arg g) f
      where
--      g :: a -> Ct r b (Cannot uncomment types because rigid type variables)
        g a = Cont h
          where
--           h :: (b -> r) -> r
             h bF = f a
-- LINK 16