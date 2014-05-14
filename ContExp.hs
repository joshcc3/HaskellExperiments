module ContExp where

import Control.Monad
import MonadInstances

-- LINK 15

-- LINK 14

-- LINK 13


cT v a
 = runCont (callCC f) print
  where
    f exit1 
     = do
        when (v <  0) $ do { exit1 "V less than zero"; return ()}
        when (v <  10 && v > 0) $ do { exit1 $ ("Square root of v is " ++) $ show $ sqrt $ v;
                              return () }
        when (v == 0) 
          (do { 
            callCC $ 
             \exit2 -> do {
                 when (a < 0) $ do { exit1 ("A is less than 0"); return () };
                 when (a < 10) $ do { exit2 ("A is less than 10"); return () };
                 exit1 $ ("The value of a - 10 is " ++) $ show $ a - 10;
               };
          } >> return ())
        return "V is 0 and A is greater than 0 but A is lesser than 10"
               

main v a 
  = print "Entering cT now" >> cT v a  >> print "Done"
    


-- LINK 17
divExcpt :: Int -> Int -> (String -> Ct r Int) -> Ct r Int
divExcpt n d handler
 = callCC $
   \ok -> do {
      err <- callCC $ 
      \notOk -> do {
          when (d == 0) $ notOk "Denominator is zero";
          ok $ div n d
      };
      handler err
   }


data Error = Error deriving (Eq, Ord, Show)

-- LINK 18
try :: ((Error -> Ct r b) -> Ct r ()) -> (Error -> Ct r b) -> Ct r ()
try m handler
 = callCC $ \ok -> do
    err <- (callCC (\notOk -> m handler >> ok ()))
    handler err
    return ()