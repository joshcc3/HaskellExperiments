{-# LANGUAGE FlexibleInstances #-}

module LambdaCalculus where

import qualified Data.Map as M
import Data.Monoid

type TypeVar = Int

data Type = Phi TypeVar | Arrow Type Type deriving (Eq, Ord, Show)

data Var = Var (Char, Int) deriving (Eq, Ord, Show)

data LambdaTerm = V Var | App LambdaTerm LambdaTerm | Abs Var LambdaTerm deriving (Eq, Ord, Show)

type Substitution = LazyMap TypeVar Type

type Context = [(TypeVar, Type)]

data LazyMap a b = Null | Node (LazyMap a b) (a, b) (LazyMap a b) deriving (Eq, Ord, Show)


instance Monoid (LazyMap Int Type) where
  mempty = Null
  Null `mappend` m = m
  m `mappend` Null = m
  m `mappend` (Node n (a, b) n') = Node (m `mappend` n) (a, applySub m b) (m `mappend` n')

insert :: Ord a => a -> b -> LazyMap a b -> LazyMap a b
insert k a Null = Node Null (k, a) Null
insert k b (Node n (k',a') n') | k < k' = Node (insert k b n) (k', a') n'
                               | k > k' = Node n (k', a') (insert k b n')
                               | otherwise = Node n (k, b) n'

fromList :: Ord a => [(a, b)] -> LazyMap a b
fromList = foldr (uncurry insert) Null

lookin :: Ord a => LazyMap a b -> a -> b
lookin (Node n (k, a) n') k' | k < k' = lookin n' k'
                             | k > k' = lookin n k'
                             | otherwise = a

union :: LazyMap a b -> LazyMap a b -> (LazyMap a b, LazyMap a b)
union Null m = (Null, m)
union m Null = (Null, m)
union (Node n (k, v) n') (Node x (k', v') x') = undefined

containsTypeVar :: Int -> Type -> Bool
containsTypeVar t (Phi t') = t == t'
containsTypeVar t (Arrow a b) = containsTypeVar t a || containsTypeVar t b

unify :: Type -> Type -> M.Map TypeVar Type
unify (Phi t) (Phi t') = M.fromList [(t, Phi t')]
unify (Phi t) b | not (containsTypeVar t b) = M.fromList [(t, b)]
                | otherwise = error "Impossible to Unify"
unify a (Phi t) = unify (Phi t) a
unify (Arrow a b) (Arrow c d) = unify a c `M.union` unify b d

applySub :: Substitution -> Type -> Type
applySub s (Phi t) = lookin s t
applySub s (Arrow a b) = Arrow (applySub s a) (applySub s b)

unifyContexts :: Context -> Context -> Substitution
unifyContexts [] _ = Null
unifyContexts ((tV, t):cs) c = undefined


--f :: [a] -> Int
f [] = 0
f x = f (replicate (length x) (length x))
