{-# LANGUAGE OverloadedStrings #-}

import Control.Monad (forM_)
import Text.Highlighting.Kate
import Text.Blaze.Html.Renderer.String (renderHtml)
import Text.Blaze.Html (toHtml)
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A

main :: IO ()
main = 
  putStrLn $ 
   renderHtml $ do
         H.head $ do
           H.style ! A.type_  "text/css" $
            toHtml $ styleToCss tango
--           H.link ! A.rel "stylesheet" ! A.type_ "text/css" ! 
--            A.href "../../../../mystyle.css"
           H.title "Using Yesod" -- $
         H.body $ do
           H.h1 "Intial Commit"
           p "This is a first commit on the blog just to see how it runs and what capabilities I can support on it. I'm going to embed a bit of haskell code just to check it out"
           toHaskell "type Cont r a = (a -> r) -> r"
           p "The above is a stream type."
           H.span ! A.class_ "dt" $ "Cont"
           p "Which bears close resemblance to"
           toHaskell "data Stream a = Cons a (Stream a)"
           br >> br
           
           "The rest of the page"
           br
           br
           "And another line"


toHaskell :: String -> Html
toHaskell = toHtml
            . formatHtmlBlock defaultFormatOpts
            . highlightAs "haskell"
