{-# LANGUAGE OverloadedStrings #-}

module Templates.Tmp where

import Control.Monad (forM_)
import Text.Highlighting.Kate
import Text.Blaze.Html.Renderer.String (renderHtml)
import Text.Blaze.Html (toHtml)
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A

genStuff :: Html
genStuff = do
         H.head $ do
           H.style ! A.type_  "text/css" $
            toHtml $ styleToCss tango
--           H.link ! A.rel "stylesheet" ! A.type_ "text/css" ! 
--            A.href "../../../../mystyle.css"
           H.title "Natural Numbers" -- $
         H.body $ do
           toHtml
            $ formatHtmlBlock defaultFormatOpts
            $ highlightAs "haskell" "data Cont = Cont { (a -> r) -> r }"
           "The rest of the page"
           br
           br
           "And another line"


--main = putStrLn $ styleToCss tango

{-
numbers n = renderHtml $ do
              (H.head $
                H.style ! A.type_ "Text" $ 
                H.title "Natural numbers")
              body $ do
                p "A list" 
                ul $ forM_ [1..n] (li . toHtml)

main = undefined
  

-}
