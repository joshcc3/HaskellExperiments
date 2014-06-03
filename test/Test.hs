module Test where

import Control.Monad

import MonadInstances


func :: Int -> Ct Int Int
func a = Cont (\c -> c (a+1))

func' :: Int -> ContT () IO Int
func' a 
  = ContT (\k -> do
    print $ "Hello" ++ (show a)
    k (a+1))


lift :: Monad m => m a -> ContT r m a
lift = ContT.(>>=)

--(( a -> ContT r m b) -> ContT r m a)

func'' :: Int -> Int -> (String -> IO ()) -> ContT () IO Int
func'' n d handler
  = callCC $ \k -> do
      lift $ print "Performing division"
      when (d == 0) $ k 0 >> return ()
      lift $ print ("Division completed successfully" ++ (show (div n d)))     
      return 1