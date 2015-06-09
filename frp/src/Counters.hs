{-# LANGUAGE ScopedTypeVariables #-} -- allows "forall t. Moment t"

import Graphics.UI.WX hiding (Event)
import Graphics.UI.WXCore as WXCore
import Reactive.Banana
import Reactive.Banana.WX
import System.Random


main :: IO ()
main = start $ do
    f       <- frame [text := "Two Counters"]
    bup     <- button f [text := "Up"]
    bdown   <- button f [text := "Down"]
    bswitch <- button f [text := "Switch Counters"]
    out1    <- staticText f []
    out2    <- staticText f []

    set f [layout := margin 10 $
            column 5 [row 5 [widget bup, widget bdown, widget bswitch],
                      grid 5 5 [[label "First Counter:" , widget out1]
                               ,[label "Second Counter:", widget out2]]]]
    

    let networkDescription :: forall t. Frameworks t => Moment t ()
        networkDescription = do
                 upE <- event0 bup command
                 dwE <- event0 bdown command
                 swE <- event0 bswitch command
                 let 
                     upE' = (+1) <$ upE
                     dwE' = (+ (-1)) <$ dwE
                     es = upE' `union` dwE'
                     es1 = whenE swB1 es
                     es2 = whenE swB2 es
                     swB1 = accumB False (not <$ swE)
                     swB2 = not  <$> swB1
                     c1 = accumB 0 es1
                     c2 = accumB 0 es2
                 sink out1 [text :== show <$> c1]
                 sink out2 [text :== show <$> c2]

    network <- compile networkDescription
    actuate network
