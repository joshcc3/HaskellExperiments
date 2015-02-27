{-# LANGUAGE ScopedTypeVariables #-} -- allows "forall t. Moment t"

import Control.Monad
import Control.Arrow
import Graphics.UI.WX hiding (Event)
import Reactive.Banana
import Reactive.Banana.WX
import Data.Char
import Data.Functor 

{-----------------------------------------------------------------------------
    Main
------------------------------------------------------------------------------}

main = switchCounters


counter :: IO ()
counter = start $ do
    f       <- frame [text := "Counter"]
    bup     <- button f [text := "Up"]
    bdown   <- button f [text := "Down"]
    output  <- staticText f []
    
    set f [layout := margin 10 $
            column 5 [widget bup, widget bdown, widget output]]
   
    let networkDescription :: forall t. Frameworks t => Moment t ()
        networkDescription = do
           eup   <- event0 bup command
           edown <- event0 bdown command
           let
             counter :: Behavior t Int
             counter = accumB 0 $ ((+1) <$ eup) `union` (subtract 1 <$ edown)
    
           sink output [text :== show <$> counter] 

    network <- compile networkDescription    
    actuate network


currencyConverter :: IO ()
currencyConverter = start $ do
   f <- frame [text := "Currency Currenter"]
   input1 <- entry f []
   input2 <- entry f []
   set f [layout := margin 10 $ column 10 
                 [grid 10 10 [[label "Dollar:", widget input1],
                              [label "Euro:", widget input2]]]]
   focusOn input1
   let networkD :: forall t. Frameworks t => Moment t ()
       networkD = do
         i1 <- behaviorText input1 ""
         i2 <- behaviorText input2 ""
         sink input1 [text :== 
                      (showNumber . fmap euroToDollar . readNum) 
                      <$> i2]
         sink input2 [text :== 
                      (showNumber . fmap dollarToEuro . readNum) 
                      <$> i1]
   network <- compile networkD
   actuate network

   where 
     dollarToEuro :: Int -> Int
     dollarToEuro = (`div` 5) . (* 3) 
     euroToDollar :: Int -> Int
     euroToDollar = (`div` 3) . (* 5)
               
          
switchCounters :: IO ()
switchCounters = start $ do
   f <- frame [text := "Two Counters"]
   bup <- button f [text := "Up"]
   bdn <- button f [text := "Down"]
   switch <- button f [text := "Switch Counters"]
   c1 <- staticText f []
   c2 <- staticText f []
   set f [layout := margin 10 $ column 10
          [grid 10 10 [[widget bdn],
                       [widget bup],
                       [label "Counter 1:", widget c1],
                       [label "Counter 2:", widget c2],
                       [widget switch]]]]
   let   networkD :: forall t. Frameworks t => Moment t ()
         networkD = do
           eup <- event0 bup command
           edown <- event0 bdn command
           eswitch <- event0 switch command

           let 
               pipeline =     join (***) (uncurry whenE) 
                          >>> uncurry union 
                          >>> accumB 0 
                          >>> fmap show
               control = accumB True (not <$ eswitch)
               control' = fmap not control
               eup' = (+1) <$ eup
               edown' = subtract 1 <$ edown
               c1B = ((control, eup'), (control', edown'))
               c2B = ((control, edown'), (control', eup'))

           sink c1 [text :== pipeline c1B]
           sink c2 [text :== pipeline c2B]
   network <- compile networkD
   actuate network
                


{-
  I would like
-}                

arithmetic :: IO ()
arithmetic = start $ do
  f <- frame [text := "Arithmetic"]
  input1 <- entry f [text := "default"]
  input2 <- entry f [text := "default"]
  output <- staticText f []
  set f [layout := margin 10 $ row 10
                    [widget input1, label "+", widget input2, 
                     label "=", minsize (sz 40 20) (widget output)]]
  
  let networkD :: forall t. Frameworks t => Moment t ()
      networkD = do
        i1 <- behaviorText input1 "asd" 
        i2 <- behaviorText input2 "asd"
        let result :: Behavior t (Maybe Int)
            result = (liftA2 . liftA2) (+) (readNumber <$> i1) (readNumber <$> i2)
            readNumber = readNum
        sink output [text :== showNumber <$> result]
  network <- compile networkD
  actuate network

showNumber Nothing = " -- "
showNumber (Just v) = show v

readNum :: String -> Maybe Int
readNum = readNum' 0
    where 
      readNum' r [] = Nothing
      readNum' r (d:ds) 
          | isDig d && null ds = Just $ r*10 + toDig d
          | isDig d = readNum' (r*10 + toDig d) ds
          | otherwise = Nothing
          where 
            isDig d = ord d >= ord '0' && ord d <= ord '9'
            toDig d = ord d - ord '0'
