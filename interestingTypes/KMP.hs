{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

import Data.Functor.Foldable
import qualified Data.Map as M
import qualified Data.Vector as V
import qualified Data.Set as S
import Data.Monoid
import Data.List
import Data.Char
import Control.Monad
import Control.Applicative
import Control.Lens
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Free
import System.Random
import Test.QuickCheck

al (Just x) = x
fi = fromIntegral

kmp :: Eq a => [a] -> [a] -> Bool
kmp = impl2

{-
  So in implementation 1 what we are going to do is 
  first build the table and then use the table to 
  check if the needle is present within the haystack in O(n).
  where n is the size of the input string. 
  So the ith element of the table will tell you which position of 
  the table to jump to should the ith character of the needle
  fail to match the tracked character of the haystack assuming that
  you have successfully matched all characters upto that ith character
-}

type KMP a = [(a, Int)]


impl1 :: forall a. Eq a => [a] -> [a] -> Bool
impl1 needle haystack = tryMatch 0 haystack
    where 
      tryMatch j []
               | j == len = True
               | otherwise = False
      tryMatch j (x:xs)
               | j < 0 = tryMatch 0 xs
               | j == len = True
               | x == needle !! j = tryMatch (j+1) xs
               | otherwise = tryMatch ((tbl !! j) + 1) (x:xs)
      tbl = tblp needle
      len = length needle
{-
  The way one generally builds this table is to keep the current index i
  you are at in the table  and another index j 
  such that [0..j] is the longest prefix matching a suffix ending at i-1
-}

tblp [] = []
tblp needle = -1:unfoldr g (1, -1)
    where 
      g :: (Int, Int) -> Maybe (Int, (Int, Int))
      g (i, j) | i == len = Nothing
               | needle !! i == needle !! (j+1) = Just (j+1, (i+1, j+1))
               | otherwise = Just (f j, (i+1, f j))
               where 
                 f x = if x >= 0 
                        then if (needle !! i) == needle !! x 
                             then x
                             else f (tblp needle !! x)
                        else x
      len = length needle
                           


{-
  I'd say that was pretty horrible. All this low level inspection of data is quite grungy and frankly quite unfit for civilzed society I say. It calls for a more declarative approach. 
  The problems with the above is the implementation of tblp. 
  If we think about it, we would like to have something that doesn't actually 
  commit to anything until it knows for sure that this is the right path. 
  
-}


{-
  So instead we could declaratively construct our table.
  Assume we have a transition function that will take us to
  the right state if the input doesn't match to
  and to the same state if it does.
  If we have exhausted our input then we are done
  If we have outstanding characters and the next character
  matches the character we are at then we get the state we 
  were a
  the transition function at this point
-}


data Kmp a = Next { nxt :: (a -> Kmp a), done :: Bool}

impl2 :: Eq a => [a] -> [a] -> Bool
impl2 n h = match table h
    where 
      match k [] = done k
      match k (x:xs) = done k || match (nxt k x) xs
      table = tbl2 n (const table)

tbl2 :: forall a. Eq a => [a] -> (a -> Kmp a) -> Kmp a
tbl2 [] f = Next f True
tbl2 (x:xs) transition = Next g False

    where 
      g a | x == a = tbl2 xs (nxt (transition x))
          | otherwise = transition x
              
match (tbl2 "aab" (const table)) "aaab"
match (tbl2 "ab" (nxt table)) "aab"
match (nxt ((tbl2 "ab" (nxt table))) 'a') "ab"
match (tbl2 "b" (nxt (nxt table 'a'))) "ab"
match (nxt (tbl2 "b" (nxt (nxt table 'a'))) 'a') "b"
match ((nxt (nxt table 'a')) 'b') "b"
match (nxt ((nxt (nxt table 'a')) 'b') 'b') []







naive needle haystack = any ((== needle) . take len) suffixes
    where 
      suffixes = tails haystack
      len = length needle

prop1 n h = kmp n h == naive n h

test = quickCheckWith myArgs

myArgs = stdArgs { maxSuccess = 750, maxSize = 750 }
