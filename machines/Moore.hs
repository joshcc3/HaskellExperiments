module Moore where

import Control.Applicative
import Data.Profunctor
import Control.Comonad

data Moore a b = Moore b (a -> Moore a b)

step a (Moore _ f) = f a

instance Functor (Moore a) where
    fmap f (Moore b g) = Moore (f b) (fmap f . g)

instance Applicative (Moore a) where
   pure x = Moore x (const (pure x))
   Moore b f <*> Moore b' f' = Moore (b b') ((<*>) <$> f <*> f')

instance Monad (Moore a) where
    return = pure
    m >>= g = joinMoore (fmap g m)

joinMoore :: Moore a (Moore a b) -> Moore a b
joinMoore m = Moore b' f''
    where 
      Moore (Moore b' _) _ = m
      f'' a = joinMoore (step a m')
          where 
            m' = stepWorlds a m

instance Comonad (Moore a) where
    extract (Moore b _) = b
    duplicate m@(Moore b f) = Moore m f
        where 
          f a = duplicate (step a m)

instance (Show a, Show b) => Show (Moore a b) where
    show (Moore b f) = "Moore " ++ show b ++ " (Thunk) "

-- the comonad instance allows us to create a moore machine
-- which tabulates all the states that machine can be in 
-- and the path to that point is the machine progressed to that state
-- It may act like a memoised datastructure
-- we could think of a moore machine as a trie or a BST


-- This function takes all worlds simultaneously one move forward
stepWorlds :: a -> Moore a (Moore a b) -> Moore a (Moore a b)
stepWorlds a (Moore b f) = Moore b' f''
    where 
      b' = step a b
      f'' = fmap (step a) . f

instance Profunctor Moore where
    lmap f (Moore c f') = Moore c g
        where 
          g = lmap f . f' . f
    rmap = fmap
        
accum :: Moore a [a]
accum = go []
    where 
      go :: [a] -> Moore a [a]
      go l = Moore l (go . (:l))

delay a (Moore a' f) = Moore a (delay a' . f)

accuml :: Moore b a -> Moore b [a]
accuml m = go [] m
    where 
--      go :: [a] -> Moore b a -> Moore b [a]
      go l (Moore a f) = Moore (a:l) (go (a:l) . f)

identity :: a -> Moore a a
identity a = Moore a identity 

driveMoore :: [a] -> Moore a b -> [b]
driveMoore l m = go l m
    where 
      go [] (Moore b _) = [b]
      go (l:ls) (Moore b f) = b:go ls (f l)
