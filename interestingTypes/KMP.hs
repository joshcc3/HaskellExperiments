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

import qualified Control.Category as C
import Data.Functor.Foldable
import qualified Data.Map as M
import qualified Data.Vector as V
import qualified Data.Set as S
import Data.Monoid
import Prelude hiding (id)
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
import Control.Arrow

al (Just x) = x
fi = fromIntegral

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

data KMP s b = KMP { runKMP :: s -> (b, KMP s b) }

instance C.Category KMP where
    id = KMP (\x -> (x, C.id))
    KMP f . KMP g = KMP h
        where
          h a = case g a of
           (b, k) -> case f b of
                  (c, k') -> (c, k' C.. k)

instance Arrow KMP where
  arr f = KMP (\s -> (f s, arr f))
  first (KMP f) = KMP (\(b, d) -> 
                    case f b of 
                      (c, k) -> ((c, d), first k))


instance Functor (KMP s) where
    fmap f (KMP g) = KMP $ \s -> case g s of
                                  (a, k) -> (f a, fmap f k)

instance Applicative (KMP s) where
    pure = def
    KMP f <*> KMP g = KMP $ \s 
                    -> case f s of
                        (a, k) -> case g s of
                                   (a', k') -> (a a', k <*> k')

instance Monad (KMP s) where
  return = pure
  k >>= f = joinKMP (fmap f k)

def x = KMP (const (x, def x))

km :: Eq a => [a] -> [a] -> Bool
km sub super =
    null
    . last
    . map snd
    . take (length super + 1)
    . iterateMachine (super, sub)
    $ kmpFunc

iterateMachine :: s -> KMP s s -> [s]
iterateMachine s k = s : uncurry iterateMachine (runKMP k s)

kmpFunc :: Eq a => KMP ([a], [a]) ([a], [a])
kmpFunc = join k
    where
      k = KMP f
      f (_, []) = (second (pure []), k)
      f ([], x) = (second . pure $ x, k)
      f (a:as, a':as')
        | a == a' = (arr tail *** successProc, k)
        | otherwise = (fProcs, k)

successProc = arr tail'
    where 
      tail' [] = []
      tail' x = tail x

delay :: c -> KMP b c -> Int -> KMP b c
delay c x 0 = x
delay c x n = KMP $ const . (c,) . delay c x $ n - 1

fProcs :: Eq a => KMP ([a], [a]) ([a], [a])
fProcs = fmap al 
         . foldl combine (pure Nothing) 
         . map (delay (Just ([], [])) failProc)
         $ [0..]
    where
      combine m n = KMP f
          where 
            f s = case runKMP m s of
                    (Nothing, x) -> runKMP n s 
                    y -> y
      al (Just x) = x

failProc :: Eq a => KMP ([a], [a]) (Maybe ([a], [a]))
failProc = k
    where 
      k = arr f
      f (a:as, a':as') | a == a' = Just $ (as, as')
                       | otherwise = Nothing

joinKMP k = KMP f
    where 
      f x = case runKMP k x of
              (m, n) -> (fst . runKMP m $ x, joinKMP . fmap (snd . flip runKMP x) $ n)

foldr' f a l = foldl (\y x -> f x . y) C.id l $ a

breakOut _ a [] = a
breakOut f a (x:xs) = case x of
                      Just x -> f a x
                      Nothing -> a