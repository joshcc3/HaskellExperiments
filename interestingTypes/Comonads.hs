{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

import Control.Comonad.Store
import Test.QuickCheck
import Prelude hiding ((^))
import Control.Comonad.Identity
import Data.List
import Data.Monoid
import Data.Functor.Foldable
import Control.Comonad
import Control.Monad
import Control.Applicative


{-

Comonads allow us to perform calculations of values inside a context. 
Which makes them great for folding up values.
Monads are great for perfoming actions which create some context which makes
them great for unfolding stuff

-}


{-

  So lets say we wanted to calculate the number of elements that are greater
  than a particular value in a tree

-}

data LZipT' w a = LZip' (w [a]) (w a) (w [a])

data LZipT w a = LZip [w a] (w a) [w a] deriving (Eq, Ord, Show, Functor)

type LZip = LZipT Identity 

shiftL :: Comonad w => LZipT w a -> LZipT w a
shiftL (LZip l a r) = LZip (tail l) (head l) (a:r)

shiftR :: Comonad w => LZipT w a -> LZipT w a
shiftR (LZip l a r) = LZip (a:l) (head r) (tail r)

absorb :: Comonad w => LZipT w a -> w (LZipT w a)
absorb z@(LZip _ v _) = v $> z


instance Comonad w => Comonad (LZipT w) where
    extract (LZip _ a _) = extract a
    duplicate z@(LZip l v r) 
        = LZip left (absorb z) right
          where 
            left = map absorb $ tail $ iterate shiftL z
            right = map absorb $ tail $ iterate shiftR z


addRight :: LZip Int -> Int
addRight z = extract z + extract (shiftR z)

all0s :: LZip Int
all0s = LZip inf0s one (one:inf0s)
    where 
      inf0s = repeat zero
      zero = return 0
      one = return 1


pTri :: Int -> LZip Int -> LZip Int
pTri 0 z = z
pTri n z = pTri (n-1) (extend addRight z)

--around :: Int -> LZip a -> [a]
around n z = left ++ right
    where 
      left = reverse $ map extract (repeatI n shiftL z)
      right = map extract $ tail $ repeatI n shiftR z

(^) :: (a -> a) -> Int -> a -> a
f ^ n = foldl (.) id (replicate n f)

repeatI :: Int -> (a -> a) -> a -> [a]
repeatI n f a = take n $ iterate f a

ans d n = around d (pTri n all0s)


type Conway = LZipT LZip Bool

result :: Conway -> Bool
result z | count < 2 = False
         | count == 3 = True
         | alive && count == 2 = True
         | otherwise = False
         where 
           alive = extract z
           count = foldl (\s b -> if b then (s+1) else s) 0 doa
           doa = g <$> [shiftL, shiftR, id] <*> [shiftL, shiftR, id] 
           g s s' = case s' z of
                        LZip _ z' _ -> extract (s z')

pretty :: Int -> Conway -> [String]
pretty n c@(LZip l v r) = (map . map) (\c -> if c then 'o' else ' ') b
    where 
      b :: [[Bool]]
      b =  map (around n) (take n l) 
                         ++ (tail $ map (around n) (take n r))

