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
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import qualified Data.Vector as V
import Control.Comonad.Store
import Test.QuickCheck
import Prelude hiding ((^))
import Control.Comonad.Identity
import Data.List
import Data.Monoid
import Data.Functor.Foldable hiding (Cons)
import Control.Comonad
import Control.Monad
import Control.Applicative


data Stream a = Cons a (Stream a) deriving (Functor, Eq, Ord, Show)

data Tree a = Node (Tree a) a (Tree a) deriving (Functor, Eq, Ord, Show)

data TZip a = TZip (Tree a) (Stream (Bool, Tree a)) deriving (Functor)

shiftU :: TZip a -> (Bool, TZip a)
shiftU (TZip _ (Cons (b, h) hs)) = (b, TZip h hs)

shiftL :: TZip a -> TZip a
shiftL (TZip t@(Node l v r) history) = TZip l (Cons (True, t) history)

shiftR :: TZip a -> TZip a
shiftR (TZip t@(Node l v r) history) = TZip r (Cons (False, t) history)

fork :: (a -> (a, a)) -> a -> Tree a
fork f a = res
    where 
      forkRes = f a
      res = case forkRes of
              (l, r) -> Node (fork f l) a (fork f r)

iterateS :: (a -> a) -> a -> Stream a
iterateS f a = xs where xs = Cons a . fmap f $ xs

tailS :: Stream a -> Stream a
tailS (Cons _ n) = n

headS :: Stream a -> a
headS (Cons a _ ) = a

instance Comonad TZip where

    extract (TZip (Node _ a _) l) = a

    duplicate z@(TZip _ l) = TZip (f z) list
        where 
          f = fork ((,) <$> shiftL <*> shiftR)
          list = (fmap . fmap) f $ 
                 tailS $ 
                 iterateS (shiftU . snd) (False, z)



