{-# LANGUAGE DeriveFunctor, TupleSections #-}

module Thread (yield, exit, cFork, fork) where

import Control.Monad
import Control.Monad.Trans.Free
import Data.Sequence -- Queue with O(1) head and tail operations


lift = FreeT . liftM Pure

data ThreadF next = Fork  next next
                  | Yield next
                  | Exit
                  deriving (Functor)

type Thread = FreeT ThreadF

yield :: (Monad m) => Thread m ()
yield = liftF (Yield ())

exit :: (Monad m) => Thread m r
exit = liftF Exit



cFork :: (Monad m) => Thread m Bool
cFork = liftF (Fork False True)


fork :: (Monad m) => Thread m a -> Thread m ()
fork thread = do
    child <- cFork
    when child $ do
        thread
        exit




roundRobin :: (Monad m) => Thread m a -> m ()
roundRobin t = go (singleton t)  -- Begin with a single thread
  where
    go ts = case (viewl ts) of
        -- The queue is empty: we're done!
        EmptyL   -> return ()

        -- The queue is non-empty: Process the first thread
        t :< ts' -> do
            x <- runFreeT t  -- Run this thread's effects
            case x of
                -- New threads go to the back of the queue
                Free (Fork t1 t2) -> go (t1 <| (ts' |> t2))

                -- Yielding threads go to the back of the queue
                Free (Yield   t') -> go (ts' |> t')

                -- Thread done: Remove the thread from the queue
                Free  Exit        -> go ts'
                Pure  _           -> go ts'


mainThread :: Thread IO ()
mainThread = do
    lift $ putStrLn "Forking thread #1"
    fork thread1
    lift $ putStrLn "Forking thread #1"
    fork thread2

thread1 :: Thread IO ()
thread1 = forM_ [1..10] $ \i -> do
    lift $ print i
    yield

thread2 :: Thread IO ()
thread2 = replicateM_ 3 $ do
    lift $ putStrLn "Hello"
    yield