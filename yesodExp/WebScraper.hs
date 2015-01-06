{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveFunctor #-}

module WebScraper (extractHtml) where

import Control.Monad
import Util
import qualified Data.Map as M
import Data.Monoid
import Text.Blaze.Html5
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A
import Text.XML.HXT.Core
import Data.Tree.NTree.TypeDefs
import Data.Functor.Foldable
import Text.Blaze.Html.Renderer.String
import Text.Blaze

myParser :: Base (NTree XNode) Html -> Html
myParser (NTreeF (XText s) []) = preEscapedToHtml s
myParser (NTreeF (XTag qn t) n) =  do
  foldl (!) tag (Prelude.map getAttr t) $ tagBody 
      where 
        tag = tagTbl . unXN . localPart' $ qn
        tagBody = foldl (<>) "" n 

al (Just x) = x


getAttr :: NTree XNode -> Attribute
getAttr (NTree (XAttr q) n) = attrib val
    where 
      attrib = attrTbl . unXN . localPart' $ q
      val = foldl g mempty n
      g r v = r <> getValue v

getValue :: NTree XNode -> AttributeValue
getValue (NTree (XText s) []) = toValue s
getValue (NTree (XText s) n) = mconcat $ toValue s : Prelude.map getValue n


data NTreeF a n = NTreeF a [n] deriving Functor

type instance Base (NTree a) = NTreeF a

instance Foldable (NTree a) where
    project (NTree a l) = NTreeF a l

stringToTree :: String -> IO [XmlTree]
stringToTree inp =
    runX $ readString [withParseHTML yes, withWarnings no] inp


treeToHtml :: XmlTrees -> Html
treeToHtml out =
  let (NTree _ n:_) = out in mconcat . Prelude.map (cata myParser) $ n
    

extractHtml :: String -> IO Html
extractHtml = stringToTree >=> return . treeToHtml
