module Loeb where


import Data.List
import Control.Applicative

loeb :: Functor f => f (f a -> a) -> f a
loeb f = xs where xs = fmap ($ xs) f

type Spreadsheet = [[Int] -> Int]

instance Num b => Num (a -> b) where
    (+) = liftA2 (+)
    (*) = liftA2 (*)
    fromInteger = const . fromInteger
    signum = fmap signum
    abs = fmap signum
    negate = fmap negate


ref = flip (!!)

sp1 :: Spreadsheet
sp1 = [ref 1, 3, ref 0 + ref 1 + ref 5, 2, 5, 4]

eval :: Functor f => f (f a -> a) -> f a
eval = loeb



type AdjList = [[[Int]] -> [Int]]


adj1 :: AdjList
adj1 = [deps [1, 2, 3], const [], deps [1, 3], const []]

deps l = foldl f (const []) l
    where 
      f g e = (++) <$> g <*> fmap (e:) (ref e)

adj2 :: AdjList
adj2 = [ref 1, const []]

topSort :: AdjList -> [Int]
topSort a = map fst res
    where 
      res = sortBy f $ zip [0..] $ eval a
      f x y = compare (length (snd x)) (length (snd y))
