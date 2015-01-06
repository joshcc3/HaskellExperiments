{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE ViewPatterns #-}

import Data.Monoid
import Templates.Tmp
import Yesod
import qualified Data.Text as T
import Templates.Blog
import Templates.Info
import Templates.Widgets
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html (toHtml)
import Yesod.Static
import Text.Blaze.Bootstrap
import Text.Blaze.BootstrapM

staticFiles "static"


data HelloWorld = HelloWorld 
              { getStatic :: Static }

getRootDir :: String 
getRootDir = "/Users/jrc12/Projects/Haskell/HaskellExperiments/yesodExp"

mkYesod "HelloWorld" [parseRoutes|
/ HomeR GET
/blog/#Int/#Int/#Int/#Int BArticleR GET
/blog BIndexR GET
/favicon.html FaviconR GET
/info InfoR GET
/tmp TmpR GET
/static StaticR Static getStatic
|]

instance Yesod HelloWorld where
    defaultLayout = myLayout

getFaviconR :: Handler Html
getFaviconR = defaultLayout $ do
                setTitle "My Image" 
--                addStylesheet @{StaticR css_bootstrap_css}
                toWidget [hamlet| <img src=@{StaticR imgs_favicon_png}>|]


getHomeR :: Handler Html
getHomeR = defaultLayout $ toWidget $
  \render -> do
    H.code "This is the answer"
--    "The body"




--genStuff --defaultLayout [whamlet|Hello Worldasd !|]

getBArticleR :: Int -> Int -> Int -> Int -> Handler Html
getBArticleR y m d n = -- sendFile typeHtml (toFile y m d n)
  do
    htmlInp <- lift (bArticle y m d n)
    defaultLayout $ do
     toWidget $ const htmlInp


getBIndexR = lift bIndex
getInfoR = lift info
getTmpR =  defaultLayout $ 
    toWidget $ \render -> do
    H.head $ do
      H.title "For Temporary Articles"
    H.body $ do
      p "This is it"

--widget1 :: Widget
insertFavicon render = 
      H.head $ do
                H.meta ! charset "utf-8"
                H.link ! rel "icon" ! (href $ render "favicon.ico")
                



myLayout :: Widget -> Handler Html
myLayout widget = do
  pc <- widgetToPageContent $ do
                    addStylesheet $ StaticR css_bootstrap_css
                    addStylesheet $ StaticR css_bootstrap_min_css
                    addStylesheet $ StaticR css_theme_css
                    addScriptRemote "https://ajax.googleapis.com/ajax/libs/jquery/1.11.1/jquery.min.js"
                    addScript $ StaticR js_bootstrap_min_js
                    navbar1
                    alertSuccess $ do
                                    H.strong "Ok! " 
                                    "This is necessary"
                    alertWarning $ do
                               H.strong "Well Done! " 
                               "This is important"
                    widget 

  withUrlRenderer $ do
       [hamlet| $doctype 5
                  <head> 
                      <title> #{pageTitle pc}
                      <meta charset=utf-8>
                      ^{pageHead pc}
                  <body>
                      <article>
                        <div class=container theme-showcase role=main>
                          ^{pageBody pc}|]
 


main :: IO ()
main = do
  stat@(Static settings) <- static "static"
  warp 3000 $ HelloWorld stat
