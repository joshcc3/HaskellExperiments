{-# LANGUAGE GeneralizedNewtypeDeriving #-}

 
import Control.Applicative
import Control.Monad.Cont
import Control.Monad.State


-- The CoroutineT monad is just ContT stacked with a StateT containing the suspended coroutines.
newtype CoroutineT r m a = CoroutineT {runCoroutineT' :: ContT r (StateT [CoroutineT r m ()] m) a}
    deriving (Functor,Applicative,Monad,MonadCont,MonadIO)
 
-- Used to manipulate the coroutine queue.
getCCs :: Monad m => CoroutineT r m [CoroutineT r m ()]
getCCs = CoroutineT $ lift get


putCCs :: Monad m => [CoroutineT r m ()] -> CoroutineT r m ()
putCCs = CoroutineT . lift . put

 
-- Pop and push coroutines to the queue.
dequeue :: Monad m => CoroutineT r m ()
dequeue = do
    current_ccs <- getCCs
    case current_ccs of
        [] -> return ()
        (p:ps) -> do
            putCCs ps
            p
 
queue :: Monad m => CoroutineT r m () -> CoroutineT r m ()
queue p = do
    ccs <- getCCs
    putCCs (ccs++[p])

 
-- The interface.
yield :: Monad m => CoroutineT r m ()
yield = callCC $ \k -> do
    queue (k ())
    dequeue
 

fork :: Monad m => CoroutineT r m () -> CoroutineT r m ()
fork p = callCC $ \k -> do
    queue (k ())
    p
    dequeue
 
-- Exhaust passes control to suspended coroutines repeatedly until there isn't any left.
exhaust :: Monad m => CoroutineT r m ()
exhaust = do
    exhausted <- null <$> getCCs
    if not exhausted
        then yield >> exhaust
        else return ()

 
-- Runs the coroutines in the base monad.
runCoroutineT :: Monad m => CoroutineT r m r -> m r
runCoroutineT = flip evalStateT [] . flip runContT return . runCoroutineT' . (<* exhaust)

-- newtype CoroutineT r m a = CoroutineT {runCoroutineT' :: ContT r (StateT [CoroutineT r m ()] m) a}
-- runContT :: ContT r m a -> (a -> m r) -> m r

-- flip evalStateT [] :: Monad m => StateT [a1] m a -> m a
-- runContT :: Monad m => ContT r m a -> (a -> m r) -> m r
-- flip runContT return :: Monad m' => ContT r m' a -> m' r
-- m' = StateT [a1] m
-- flip runContT return :: ContT r (StateT [a1] m) a -> (StateT [a1] m) r
-- runCoroutineT' :: CoroutineT r m a -> ContT r (StateT [CoroutineT r m ()] m) a
-- a1 = CoroutineT r m ()
-- flip runContT return :: ContT r (StateT [CoroutineT r m ()] m) a -> (StateT [CoroutineT r m ()] m) r
-- (<* exhaust) :: CoroutineT r m ()


--------------------------------------------------------------------------------

printOne :: (Show a, MonadIO m) => a -> CoroutineT r m ()
printOne n = do
    liftIO (print n)
    yield

-- LINK 19
example :: IO ()
example = runCoroutineT $ do
    fork $ replicateM_ 3 (printOne 3)
    fork $ replicateM_ 4 (printOne 4)
    replicateM_ 2 (printOne 2)
