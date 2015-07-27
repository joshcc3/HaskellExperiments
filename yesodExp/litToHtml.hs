{-# LANGUAGE OverloadedStrings #-}

import Control.Monad (forM_)
import Text.Highlighting.Kate
import Text.Blaze.Html.Renderer.String (renderHtml)
import Text.Blaze.Html (toHtml)
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A

main :: IO ()
main = do
  fn <- getLine
  c <- readFile fn
  putStrLn $ 
   renderHtml $ do
         H.head $ do
           H.style ! A.type_  "text/css" $
            toHtml $ styleToCss tango
         H.body $ do
                  toHaskell c

litToHaskell :: ([String]) -> Html
litToHaskell s = g $ foldl f ([], p "Begin" ) s
    where 
      f :: (String, Html) -> String -> (String, Html)
      f (s, h) ('>':' ':r) = (s++"\n"++r, h)
      f (s, h) o = ([], h >> toHaskell s >> p (toHtml o))
      g :: (String, Html) -> Html
      g (s, h) = h >> toHaskell s

toHaskell :: String -> Html
toHaskell = toHtml
            . formatHtmlBlock defaultFormatOpts
            . highlightAs "haskell"
