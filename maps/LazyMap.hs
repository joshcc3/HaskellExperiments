{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
module LazyMap (lookup, map, fromList, fold, 
                foldWithKey, empty, union, toLMap, keys) where

import Prelude hiding (lookup, map, (.), id)
import Control.Applicative hiding (empty)
import Control.Category
import Data.Monoid

type Set a = a -> Bool

data Arr m a b = Arr (Set a, a -> m b) 

type Map = Arr Maybe

instance Functor (Map a) where
    fmap f (Arr (l, g)) = Arr (l, fmap f . g)

instance Applicative (Map a) where
    pure a = undefined -- Arr $ (mempty, pure . pure $ a)




{-

subst (s >=> s') = subst s . subst s' -- 

s :: a -> f b
s' :: b -> f c
subst :: (a -> f b) -> f a -> f b

s >=> s' :: a -> f c

subst (s >=> s') :: a -> f c
subst s :: f a -> f b
subst s' :: f b -> f c
subst s . subst s' :: f a -> f c

So subst is a functor from the category 

So apparently, (=<<) is a functor from the Kleisel category to the category of 
functions

(a -> f b) -> f a -> f b

-}






lookup :: a -> Map a b -> Maybe b
lookup a (Arr (_, f)) = f a

map :: (b -> c) -> Map a b -> Map a c
map f (Arr (l, g)) = Arr (l, fmap f . g)

empty :: Map a b
empty = Arr (const False, const Nothing)


fromList :: forall a b. [(a, b)] -> Map a b
fromList l = foldl f empty l
    where 
      f :: Map a b -> (a, b) -> Map a b
      f (Arr (s, f)) (k, v) = Arr (s', f')
          where 
            s' = undefined
            f' a = undefined

insert :: a -> b -> Map a b -> Map a b
insert a b (Arr (s, f)) = Arr (s', f')
    where 
      f' = undefined
      s' = undefined


fold = error "Not implemented"
foldWithKey = error "Not implemented"
union = error "Not implemented"
toLmap = error "Not implemented"
keys = error "Not implemented"
toLMap = error "Not implemented"
