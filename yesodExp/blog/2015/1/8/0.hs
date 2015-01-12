{-# LANGUAGE OverloadedStrings #-}
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A
import Text.Highlighting.Kate
import Text.Blaze.Html.Renderer.String

myHtml :: Html
myHtml = do
  H.head $ 
     H.title $ "TODO List"
  H.body $ do 
     H.h1 "Stuff to take back"
     H.ul $ do
      H.li "Nandos" 
      H.li "Soup"
     H.ul  $
      H.li "Shirts"
     H.ul $
      H.li "Pens"
     


-- need to use that layout theme for my blog


main = do
  putStrLn . renderHtml $ myHtml

toHaskell :: String -> Html
toHaskell = toHtml
            . formatHtmlBlock defaultFormatOpts
            . highlightAs "haskell"
