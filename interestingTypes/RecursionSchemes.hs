{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}


import Control.Applicative
import Data.Char
import Data.Monoid
import qualified Data.Map as M
import qualified Data.Set as S
import Control.Monad
import Data.Functor.Foldable

main = undefined


{-
  Recursion schemes uses the following methods to encode recursion.
  A catamorphism maps the An algebra over the fixed point to an algebra
  over the recursive structure

-}


sumList :: Num a => Prim [a] a -> a
sumList Nil = 0
sumList (Cons a b) = a + b

byte :: Tree b -> [(Tree b, Tree b, b)]
byte = para' f
    where 
      f :: TreeF b (Tree b, [(Tree b, Tree b, b)]) -> [(Tree b, Tree b, b)]
      f NullF = []
      f (NodeF (t, a) v (t', a')) = (t, t', v) : a ++ a'


{-
para :: Foldable t => 
(Base t (t, a) -> a) -> t -> a
(TreeF b (t, a) -> a) -> Tree b -> a


Base t a
TreeF b a 
NodeF a (Tree b, b) a = (t, b)
-}


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
            g t' = (t', m t')

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

{-
The prepromorphism allows us to apply a fold but just before we fold apply a structure changing transformation. That is, every time we are about to fold up the structure it allows to perform a structure changing transformation
-}

prepro' :: (Foldable t, Unfoldable t) => (forall b. Base t b -> Base t b) -> (Base t a -> a) -> t -> a
prepro' g f = m where m = f . g . fmap m . project

--embed :: Unfoldable t -> Base t t -> t
--ana :: (a -> Base t a) -> a -> t
--ana f = m where m = f . fmap m . embed


fI :: (Num a, Integral b) => b -> a
fI = fromIntegral




{-
gunfold' :: (Unfoldable t, Monad m) => 
          (forall b. m (Base t b) -> Base t (m b))
          -> (a -> Base t (m a)) -> a -> t
What a generalized unfold does is to construct
f (f (f (f ... 
By repeatedly unfurling the functor with g, 
then push the monad inside using f, 
then flatten the monad
and now create the fixed point of the base t functor
is actually t.
The importance of flattening the monad is that
-}
gunfold' :: (Unfoldable t, Monad m, Functor m) => 
          (forall b. m (Base t b) -> Base t (m b))
          -> (a -> Base t (m a)) -> a -> t
gunfold' f g a = embed $ m (g a)
    where 
      m x = fmap (embed . m . fmap join . f . fmap g) x



{-
a
f (m a)
f (m (f (m a))
f (f (m (m a))
f (f (m a))


a
Base t (m a) = f (f' a)
Base t (m (Base t t)) = f (f' (f t))
Base t (Base t (m t)) = f (f (f' t))


Base t (Base t (Base t...) = t
f (f' a) = f (f (f' a))
-}












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




--tc st1 p1 t1 st2 p2 t2
-- = take t1 [st1, st1 + p1 ..] take t2 [st2, st2 + p2 ..]

{-
So if i want to know the largest smaller than a particular element
-}

largest :: Ord k => TreeF k (Maybe k) -> Maybe k
largest NullF = Nothing
largest (NodeF l a l') = Just $ maybe a id l'

lSmThan :: Ord k => k -> TreeF k (Maybe k) -> Maybe k
lSmThan k NullF = Nothing
lSmThan k (NodeF l k' l') | k < k' = l
                          | k > k' = Just $ maybe k' id l'
                          | otherwise = l

trial k = cata (lSmThan k)

search :: Ord k => TreeF k (k -> Bool) -> (k -> Bool)
search NullF k = False
search (NodeF t k' t') k | k' == k = True
                         | k' > k = t' k
                         | k' < k' = t k


insert' :: (Num k, Ord k) => k -> TreeF k (Either (Tree k) k)
insert' x | x <= 0 = NullF
          | x > 100 = NullF
          | otherwise = NodeF (Right (x - 3)) (x-1) (Right (x-2))

insert :: Ord k => k -> TreeF k (Tree k) -> Tree k
insert k NullF           = Node Null k Null
insert k (NodeF t k' t') | k < k' = t
                         | otherwise = t'


createTree :: Ord k => Prim [k] (Tree k) -> Tree k
createTree Nil = Null
createTree (Cons a b) = cata (insert a) b





instance Unfoldable (Tree a) where
    embed NullF = Null
    embed (NodeF t a t') = Node t a t'

ana' :: Unfoldable t => (a -> Base t a) -> a -> t
ana' f = m where m = embed . (fmap m) . f

apo' :: forall a t. Unfoldable t => (a -> Base t (Either t a)) -> a -> t
apo' f = m 
    where 
      m = embed . (fmap g) . f
      g = either id m
          

swapTree :: TreeF a b -> TreeF a b
swapTree NullF = NullF
swapTree (NodeF t a t') = NodeF t' a t


genList :: Int -> Prim [Int] (Either [Int] Int)
genList t | t > 0 = Cons (t-1) (Right (t-1))
          | otherwise = Nil


{-
  We might want to fold a tree.
  Folding of a tree works by 

  So if I want to know the largest element smaller than some other element. 
-}
