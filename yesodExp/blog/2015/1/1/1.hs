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
           toHaskell "f :: (w a -> b) -> w a -> w b\nf = undefined\n\nf :: a -> m b\n\nf = undefined\n\n\ninstance Comonad (e,) where\n  extract (_, a) = a\n  duplicate (e, a) = (e, (e, a))\n  extend (e, a) f = (e, f (e, a))\n\nreturn :: a -> r -> a\nreturnop :: a -> r -> a\nreturnop :: r -> a -> a\nreturnop :: (r, a) -> a\nextract :: (r, a) -> a\n\n(>>=) :: m a -> (a -> m b) -> m b\n(>>=)op :: w b -> (w b -> a) -> w a\nextend :: w b -> (w b -> a) -> w a\n\n\ninstance Monoid m => Comonad ((->) m) where\n  extract f = f mempty\n  duplicate m f = \\w -> \\w' -> f (w <> w')\n  extend m f = \\w -> f (\\w' -> m (w <> w'))\n\n"
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
