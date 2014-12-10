{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}


import Control.Applicative
import Data.List
import Data.Char
import Data.Monoid
import qualified Data.Map as M
import qualified Data.Set as S
import Control.Monad
import Data.Functor.Foldable


{-
  Recursion schemes uses the following methods to encode recursion.
  A catamorphism maps the An algebra over the fixed point to an algebra
  over the recursive structure

-}

myFunc2 = para f
    where 
      f (Cons a (l, b)) = (a, l) : b
      f Nil = []

myFunc = cata f 
    where 
      f (Cons a b) = a + b
      f Nil = 0


data Tree a = Node (Tree a) a (Tree a) | Null deriving (Eq, Ord, Show)

data TreeF a b = NodeF b a b | NullF deriving (Eq, Ord, Show, Functor)

type instance (Base (Tree a)) = TreeF a

-- Base is a type level function that maps a recursive type t to its functor representation.
-- Base t b - is the saturated functor type. Usually it will be the 

-- project :: Foldable t => t -> Base t t
-- project :: Foldable t => t -> Fix t
-- project :: Tree a -> TreeF a
-- Node t a t' = 


instance Foldable (Tree a) where
    project (Node t a t') = NodeF t a t'
    project Null = NullF


treeFunc = cata f
    where 
      f (NodeF b _ b') = 1 + max b b'
      f NullF = 0

{-
   Translates F-Algebra to an algebra over the
   explicit recursive type
   project :: t -> Base t t
   So we have something that can relate the
   explicit recursive type with the base functor
   now we want to go in the reverse direction
   We want to go from the explicit recursive type
   project :: t -> Base t t
-}


{-
   Right, so a catamorphism is the fixed point of a
   morphism that collapses a recursive type down to 
   a single value.
   When the base case is the base case of the recursive type
-}

cata' :: forall t a. Foldable t => (Base t a -> a) -> t -> a
cata' f = m'
    where 
      m' :: t -> a
      m' = f . fmap m' . project


{-
   para' :: Foldable t => (Base t (t, a) -> a) -> t -> a
   So a paramorphism is a morphism that collapses a functor
   with objects in the source category its own recursive type
   and an associated value. It provides a snapshot of the 
   recursive type at that point. 
   Right so a paramorphism is a function that allows us to
   look at the 
-}

doPrimList (Cons a (t, b)) = (a, t) : b
doPrimList Nil = []

para' :: forall t a. Foldable t => (Base t (t, a) -> a) -> t -> a
para' f = m
    where 
      m :: t -> a
      m t = f $ fmap g $ project t
          where 
            g t' = (t, m t')

{-
So a paramorphism allows us to fold a functor and obtain a snapshot of the functor at the time of folding
-}


{-
So a zygomorphism gives us a normal f algebra
It also gives us a an algebra with an accumulator
So the zygomorphism is where stuff starts to really 
get copmlicated and pretty sexy.
Operationally the zygomorphism does the following
it creates a morphism that
foldls a foldable with a 
morphism g that
h folds the structure according to t
g folds the structure using the 
So a zygomorphism allows you to fold a structure at a point
using the information of a fold on that structure
It builds a tower of applications and then collapses them
{-
cata f = c where c = f . fmap c project

f = fg . fmap h

m [1..10] = cata f [1..10]
c [1..10] where c x = f $ (fmap c) (Cons 1 [2..10])
f (Cons 1 (f (Cons 2 (f (Cons 3(f ...))))))
(fg . fmap h)
-}
This implementation is wrong, the zygomorphism is supposed to
provide to the second algebra the folded result based on the local context
-}
zygo' :: forall t a b. Foldable t => (Base t b -> b) -> (Base t (b, a) -> a) -> t -> a
zygo' t at = m
    where 
      m x = cata g x
          where 
            g :: Base t a -> a
            g t = at $ fmap h t
            h :: a -> (b, a)
            h a = (cata t x, a)

fI :: (Num a, Integral b) => b -> a
fI = fromIntegral

avgPrimList :: Base [Int] (Int, [Int]) -> [Int]
avgPrimList Nil = []
avgPrimList (Cons _ (old, res)) = old : res


filterGreaterThanAvg :: Base [Int] ((Int, Int), [Int]) -> [Int]
filterGreaterThanAvg Nil = []
filterGreaterThanAvg (Cons v ((l, s), fe)) = if v >= s `div` l 
                                             then v:fe
                                             else fe

lengthPrimList Nil = 0
lengthPrimList (Cons _ b) = 1 + b

paraTree NullF = []
paraTree (NodeF (t, a) v (t', a')) = (t,t'):a ++ a'

treeFunc2 = para f
    where 
      f (NodeF (t, a) v (t', a')) = t == t' && a && a'
      f NullF = True





tree4 x = Node (Node x 'q' (Node tree1 'q' x)) 'a' (Node (Node x 'e' Null) 'b' (Node (Node tree3 'c' tree2) 'd' (Node tree2 'e' Null)))

tree1 = Node (Node Null 'a' Null) 'a' Null
tree2 = Node Null 'b' (Node Null 'a' Null)
tree3 = Node Null 'b' Null


{-
cata
ana
para
prepro
futu

project
zygo
histo
hylo
and the generalized versions
-}


