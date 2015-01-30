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

data TreeF a n = Node n a n | Null deriving (Functor)

type Tree a = Fix (TreeF a) 

type TZip a = (Tree a, [(Bool, Tree a)])

bBinT :: [Int] -> Tree Int
bBinT = ana alg
    where 
      alg :: [Int] -> Base (Tree Int) [Int]
      alg [] = Null
      alg (l:ls) = Node less l greater
          where 
            less = filter (<=l) ls
            greater = filter (>l) ls

