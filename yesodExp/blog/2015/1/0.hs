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
           toHaskell "{-# LANGUAGE RankNTypes #-}\n{-# LANGUAGE TypeFamilies #-}\n{-# LANGUAGE DeriveFunctor #-}\n{-# LANGUAGE NoMonomorphismRestriction #-}\n{-# LANGUAGE ScopedTypeVariables #-}\n{-# LANGUAGE TupleSections #-}\nmodule Main where\n\nimport Data.Foldable (foldMap)\nimport Control.Applicative\nimport Data.List\nimport Data.Char\nimport Data.Monoid\nimport qualified Data.Map as M\nimport qualified Data.Set as S\nimport Control.Monad\nimport Data.Functor.Foldable\n\nforLoop :: b -> [a] -> (a -> b -> Either d (c, b)) -> Either d [c]\nforLoop _ [] _ = Right []\nforLoop st (a:as) f = \n    f a st >>= \\(c, st') -> fmap (c :) $  forLoop st' as f\n\nbreakOn :: Int -> Int -> Int -> Either Int (Int, Int)\nbreakOn x a b = if a == x\n                then Left b\n                else Right (b - a, a + b)\n\n\nmain = do\n  s <- getLine\n  _ <- forM_ [1..(read s)] $ const $ getLine >>= putStrLn . (++)\n  return ()\n  \n  \n\n{-\n  Recursion schemes uses the following methods to encode recursion.\n  A catamorphism maps the An algebra over the fixed point to an algebra\n  over the recursive structure\n\n-}\n{-\ngunfold' :: (Unfoldable t, Monad m) => \n          (forall b. m (Base t b) -> Base t (m b))\n          -> (a -> Base t (m a)) -> a -> t\nWhat a generalized unfold does is to construct\nf (f (f (f ... \nBy repeatedly unfurling the functor with g, \nthen push the monad inside using f, \nthen flatten the monad\nand now create the fixed point of the base t functor\nis actually t.\nThe importance of flattening the monad is that\n-}\ngunfold' :: (Unfoldable t, Monad m, Functor m) => \n          (forall b. m (Base t b) -> Base t (m b))\n          -> (a -> Base t (m a)) -> a -> t\ngunfold' f g a = embed $ m (g a)\n    where \n      m x = fmap (embed . m . fmap join . f . fmap g) x\n\n\n\n{-\na\nf (m a)\nf (m (f (m a))\nf (f (m (m a))\nf (f (m a))\n\n\na\nBase t (m a) = f (f' a)\nBase t (m (Base t t)) = f (f' (f t))\nBase t (Base t (m t)) = f (f (f' t))\n\n\nBase t (Base t (Base t...) = t\nf (f' a) = f (f (f' a))\n-}\n\n\n{-\nembed :: Base t t -> t\nf :: m (Base t b) -> Base t (m b)\ng :: a -> Base t (m a)\na :: \na\nBase t (m a) \nm (m x) -> m x\nx -> m x\nm a -> m t\ng a\nz = Base t (m a)\n(fmap (fmap g)) z\ny = Base t (m (Base t (m a)))\nfmap f y\nu = Base t (Base t (m (m a)))\nfmap . fmap join \nBase t (Base t (m a))\n \n-}\n\n\nsort' :: [Int] -> [Int]\nsort' l = undefined\n\nasd :: Prim [Int] (Int, [(Int, Int)]) -> [(Int, Int)]\nasd Nil = []\nasd (Cons a (v, l)) = (a, v) : l\n\ntrial :: Prim [Int] Int -> Int\ntrial Nil = 0\ntrial (Cons a l) = a + l\n\n\nsolve :: Prim [Int] ((Maybe Int, Maybe Int), [Int]) -> [Int]\nsolve Nil = []\nsolve (Cons a ((n, n'), l)) | a < 0 =  maybe (a:l) (\\x -> a:x:l) n'\n                            | otherwise =  maybe (a:l) (\\x -> a:x:l) n\n\nnextNeg :: Prim [Int] (Maybe Int) -> Maybe Int\nnextNeg Nil = Nothing\nnextNeg (Cons a b) | a < 0 = Just a\n                      | otherwise = b\nnextPos :: Prim [Int] (Maybe Int) -> Maybe Int\nnextPos Nil = Nothing\nnextPos (Cons a b) | a > 0 = Just a\n                      | otherwise = b\n\nhelper :: Prim [Int] (Maybe Int, Maybe Int) -> (Maybe Int, Maybe Int)\nhelper l = (nextNeg (fmap fst l), nextPos (fmap snd l))\n"

           "The rest of the page"
           br
           br
           "And another line"


toHaskell :: String -> Html
toHaskell = toHtml
            . formatHtmlBlock defaultFormatOpts
            . highlightAs "haskell"
