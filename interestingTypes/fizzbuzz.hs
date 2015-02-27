{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}



module FizzBuzz where

import qualified Data.Map as M
import qualified Data.Vector as V
import qualified Data.Set as S
import Data.Monoid
import Data.Char
import Control.Monad
import Control.Applicative
import Control.Lens
import Test.QuickCheck
import Control.Comonad

al (Just x) = x
fi = fromIntegral


data Stream a = a :> Stream a deriving (Functor, Eq, Ord, Show)

tailS (_:> t) = t
headS (a:>_) = a

iterateS :: (a -> a) -> a -> Stream a
iterateS f a = a :> iterateS f (f a)

data LZip a = L (Stream a, a, Stream a) deriving (Functor, Eq, Ord, Show)

shiftL :: LZip a -> LZip a
shiftL (L (l, a, r)) = L (tailS l, headS l, a:> r)

shiftR :: LZip a -> LZip a
shiftR (L (l, a, r)) = L (a:>l, headS r, tailS r)

instance Comonad LZip where
    extract (L (_, a, _)) = a
    duplicate l = L (tailS $ iterateS shiftL l, headS left, right)
                         where 
                           left = iterateS shiftL l
                           right = tailS $ iterateS shiftR l


divx msg x n | n `mod` x == 0 = msg
             | otherwise = ""


fizzBuzz :: [Int -> String] -> Int -> String
fizzBuzz = mconcat

pairs = [("fizz", 3), ("buzz", 5), ("hiss", 7), ("wazz", 11)]


soln = do
  res <- fizzBuzz (map (uncurry divx) pairs)
  if null res
  then show
  else return res

filterMa :: Monad m => (a -> m Bool) -> [a] -> m [a]
filterMa f [] = return []
filterMa f (a:as) = liftA2 (++) (f a) (filterMa f as)
