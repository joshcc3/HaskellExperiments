{-# LANGUAGE OverloadedStrings #-}

module Util where

import Data.String
import Text.Blaze.Internal
import qualified Data.Set as S
import Text.Blaze.Html5
import qualified Data.Map as M
import Text.Blaze.Html5.Attributes as A 

data HtmlVar = HtmlVar { version :: [String], docT :: [String],
                         parents :: S.Set String, leafs ::  S.Set String,
                         attributes :: S.Set String }



html5 :: HtmlVar
html5 = HtmlVar
    { version = ["Html5"]
    , docT = ["<!DOCTYPE HTML>"]
    , parents =
        S.fromList $ [ "a", "abbr", "address", "article", "aside", "audio", "b"
        , "bdo", "blockquote", "body", "button", "canvas", "caption", "cite"
        , "code", "colgroup", "command", "datalist", "dd", "del", "details"
        , "dfn", "div", "dl", "dt", "em", "fieldset", "figcaption", "figure"
        , "footer", "form", "h1", "h2", "h3", "h4", "h5", "h6", "head", "header"
        , "hgroup", "html", "i", "iframe", "ins", "kbd", "label"
        , "legend", "li", "map", "mark", "menu", "meter", "nav", "noscript"
        , "object", "ol", "optgroup", "option", "output", "p", "pre", "progress"
        , "q", "rp", "rt", "ruby", "samp", "script", "section", "select"
        , "small", "span", "strong", "style", "sub", "summary", "sup"
        , "table", "tbody", "td", "textarea", "tfoot", "th", "thead", "time"
        , "title", "tr", "ul", "var", "video"
        ] ++ [ "applet", "center", "dir", "font", "iframe", "isindex", "menu"
        , "noframes", "s", "u", "frameset"
        ] 
    , leafs =
        -- http://www.whatwg.org/specs/web-apps/current-work/multipage/syntax.html#void-elements
        S.fromList $ [ "area", "base", "br", "col", "embed", "hr", "img", "input", "keygen"
        , "link", "menuitem", "meta", "param", "source", "track", "wbr"
        ] ++ ["basefont", "frame"]
    , attributes =
        S.fromList  $ [ "frameborder", "scrolling", "accept", "accept-charset", "accesskey", "action", "alt", "async"
        , "autocomplete", "autofocus", "autoplay", "challenge", "charset"
        , "checked", "cite", "class", "cols", "colspan", "content"
        , "contenteditable", "contextmenu", "controls", "coords", "data"
        , "datetime", "defer", "dir", "disabled", "draggable", "enctype", "for"
        , "form", "formaction", "formenctype", "formmethod", "formnovalidate"
        , "formtarget", "headers", "height", "hidden", "high", "href"
        , "hreflang", "http-equiv", "icon", "id", "ismap", "item", "itemprop"
        , "keytype", "label", "lang", "list", "loop", "low", "manifest", "max"
        , "maxlength", "media", "method", "min", "multiple", "name"
        , "novalidate", "onbeforeonload", "onbeforeprint", "onblur", "oncanplay"
        , "oncanplaythrough", "onchange", "oncontextmenu", "onclick"
        , "ondblclick", "ondrag", "ondragend", "ondragenter", "ondragleave"
        , "ondragover", "ondragstart", "ondrop", "ondurationchange", "onemptied"
        , "onended", "onerror", "onfocus", "onformchange", "onforminput"
        , "onhaschange", "oninput", "oninvalid", "onkeydown", "onkeyup"
        , "onload", "onloadeddata", "onloadedmetadata", "onloadstart"
        , "onmessage", "onmousedown", "onmousemove", "onmouseout", "onmouseover"
        , "onmouseup", "onmousewheel", "ononline", "onpagehide", "onpageshow"
        , "onpause", "onplay", "onplaying", "onprogress", "onpropstate"
        , "onratechange", "onreadystatechange", "onredo", "onresize", "onscroll"
        , "onseeked", "onseeking", "onselect", "onstalled", "onstorage"
        , "onsubmit", "onsuspend", "ontimeupdate", "onundo", "onunload"
        , "onvolumechange", "onwaiting", "open", "optimum", "pattern", "ping"
        , "placeholder", "preload", "pubdate", "radiogroup", "readonly", "rel"
        , "required", "reversed", "rows", "rowspan", "sandbox", "scope"
        , "scoped", "seamless", "selected", "shape", "size", "sizes", "span"
        , "spellcheck", "src", "srcdoc", "start", "step", "style", "subject"
        , "summary", "tabindex", "target", "title", "type", "usemap", "value"
        , "width", "wrap", "xmlns"
        ] ++  [ "background", "bgcolor", "clear", "compact", "hspace", "language"
        , "noshade", "nowrap", "start", "target", "vspace"
        ]
   }

tagTbl :: String -> Html -> Html
tagTbl s | S.member s (parents html5) = Parent (fromString s) (fromString $ "<"++s)
                                        (fromString $ "</" ++ s ++ ">")
         | S.member s (leafs html5) = const $ Leaf (fromString s) (fromString $ "<" ++ s)
                                      ">"
         | otherwise = Parent (fromString s) (fromString $ "<"++s)
                                        (fromString $ "</" ++ s ++ ">")

attrTbl :: String -> (AttributeValue -> Attribute)
attrTbl s | S.member s (attributes html5) = attribute (fromString s) (fromString $ " " ++ s ++ "=\"")
          | otherwise = customAttribute (fromString s)


