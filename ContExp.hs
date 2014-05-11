module ConExp where

import Control.Monad
import MonadInstances

-- LINK 13

{-newtype Ct r a = Cont {runCont :: (a -> r) -> r}

instance Monad (Ct r) where
  return arg = Cont ($arg)

  (>>=) g f = Cont (\c -> runCont g (\a -> runCont (f a) c))
-}

database = [(1, 12), (2, 29)]


-- lets say we want to create a function that can return early from a
-- function and go somewhere else

f :: Int -> (Int -> IO ()) -> IO ()
f v k 
  = do 
     Just x <- return $lookup v database
     when (x > 5) $ do { k x; (print "Exiting"); return () }
     return ()
     

throw :: Int -> IO ()
throw v = (print $ "Error code " ++ (show v))


f' :: Int -> Ct (IO ()) Int
f' v = Cont (\k -> do { when (v > 0) (k v); when (v < 0) $ print "Less than 0"})
               