{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE ViewPatterns #-}


module Templates.Blog where

import Text.Pandoc
import Text.Blaze.Html5 as H
import Control.Arrow
import Yesod
import Text.Blaze.Html
import Text.Blaze.Renderer.String


--getHomeR :: Handler Html
type Year = Int
type Month = Int
type Day = Int
type No = Int

toFile :: Year -> Month -> Day -> No -> FilePath
toFile y m d n = Prelude.concat ["./blog/", show y, "/", show m,  "/", show d,  "/", show n, ".html"] 
bArticle :: Year -> Month -> Day -> No -> IO Html
bArticle y m d n = do
  inp <- readFile $ toFile y m d n 
  return $ writeHtml def {writerHtml5 = True } . readHtml def $ inp


bIndex :: IO Html
bIndex = fmap toHtml $ readFile "./blog/index.html"

getTmp :: IO Html
getTmp = undefined



