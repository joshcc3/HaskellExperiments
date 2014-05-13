module ContExp where

import Control.Monad
import MonadInstances

-- LINK 15

-- LINK 14

-- LINK 13


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


fun :: Int -> String
fun n = (`runCont` id) $ do
    str <- callCC $ \exit1 -> do                            -- define "exit1"
        when (n < 10) (exit1 (show n))
        let ns = map digitToInt (show (n `div` 2))
        n' <- callCC $ \exit2 -> do                         -- define "exit2"
            when ((length ns) < 3) (exit2 (length ns))
            when ((length ns) < 5) (exit2 n)
            when ((length ns) < 7) $ do
                 exit1 ("A")    --escape 2 levels
            return $ length ns
        return $ "(ns = " ++ (show ns) ++ ") " ++ (show n')
    return $ "Answer: " ++ str


digitToInt :: Char -> Int
digitToInt '0' = 0
digitToInt '1' = 1

intToDigit :: Int -> Char
intToDigit 0 = '0'
{- This is a bogus function that is present simply to test the Continuation monad
   When v < 0, it returns    
-}
cT v a
 = runCont (callCC f) print
  where
    f exit1 
     = do
        when (v <  0) $ do { exit1 "V less than zero"; return ()}
        when (v <  10 && v > 0) $ do { exit1 $ ("Square root of v is " ++) $ show $ sqrt $ v*v; 
                              return () }
        when (v == 0) 
          (do { 
            callCC $ \exit2 -> 
               do {
                 when (a < 0) $ do { exit1 ("A is less than 0"); return () };
                 when (a < 10) $ do { exit2 ("A is less than 10"); return () };
                 exit1 $ ("The value of a - 10 is " ++) $ show $ a - 10;
               };
          } >> return ())
        return "V is 0 and A is greater than 0 but A is lesser than 10"
               