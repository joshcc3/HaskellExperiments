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

module Conway(LZip, shiftL, shiftR, crop, crop2D, pretty) where

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

data LZip a = LZip [a] a [a] deriving (Eq, Ord, Show, Functor)

shiftL :: LZip a -> LZip a
shiftL (LZip l a r) = LZip (tail l) (head l) (a:r)

shiftR :: LZip a -> LZip a
shiftR (LZip l a r) = LZip (a:l) (head r) (tail r)

instance Comonad LZip where
    extract (LZip _ a _) = a
    duplicate z@(LZip l v r)
        = LZip left z right
          where
            left = tail $ iterate shiftL z
            right = tail $ iterate shiftR z

--around :: Int -> LZip a -> [a]
around n z = left ++ right
    where
      left = reverse $ map extract (repeatI n shiftL z)
      right = map extract $ tail $ repeatI n shiftR z

(^) :: (a -> a) -> Int -> a -> a
f ^ n = foldl (.) id (replicate n f)

repeatI :: Int -> (a -> a) -> a -> [a]
repeatI n f a = take n $ iterate f a

type Conway = LZip (LZip Bool)

result :: Conway -> Bool
result z | count < 2 = False
         | count == 3 = True
         | alive && count == 2 = True
         | otherwise = False
         where
           alive = extract (extract z)
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

crop :: Int -> LZip a -> [a]
crop n z = (reverse $ map extract left) ++ (focus:map extract right)
    where 
      left = take n (tail $ iterate shiftL z)
      right = take n (tail $ iterate shiftR z)
      focus = extract z

crop2D :: Int -> LZip (LZip a) -> [[a]]
crop2D n = map (crop n) . crop n


