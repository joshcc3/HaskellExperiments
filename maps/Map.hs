module Map where

import Control.Applicative
import Prelude hiding (lookup, map)
import Data.Semigroup
import qualified Data.Map as M

newtype LazyMap a b =  LMap (a -> Maybe b, M.Map a b)


instance (Show a, Show b) => Show (LazyMap a b) where
    show (LMap (f, m)) = concat ["LMap", "(Thunk, ", show m, ")"]

lookup :: Ord a => a -> LazyMap a b -> Maybe b
lookup a (LMap (f, m)) = M.lookup a m <|> f a

map :: (b -> c) -> LazyMap a b -> LazyMap a c
map f (LMap (g, m)) = LMap (fmap f . g, M.map f m)

fromList :: Ord a => [(a, b)] -> LazyMap a b
fromList l = LMap (const Nothing, M.fromList l)

fold :: (a -> b -> b) -> b -> LazyMap k a -> b
fold f b (LMap (_, m)) = M.fold f b m

foldWithKey :: (k -> a -> b -> b) -> b -> LazyMap k a -> b
foldWithKey f b (LMap (_, m)) = M.foldWithKey f b m

empty :: LazyMap a b
empty = LMap (const Nothing, M.empty)

insert :: Ord a => a -> b -> LazyMap a b -> LazyMap a b
insert a v (LMap (f, b)) = LMap (f, M.insert a v b)

union :: (Semigroup b, Ord a) => LazyMap a b -> LazyMap a b -> LazyMap a b
union (LMap (f, m)) (LMap (f', m')) = LMap (liftA2 (<>) f f', m `M.union` m')

toLMap :: (a -> Maybe b) -> M.Map a b -> LazyMap a b
toLMap = curry LMap


keys :: LazyMap a b -> [a]
keys (LMap (_, m)) = M.keys m
