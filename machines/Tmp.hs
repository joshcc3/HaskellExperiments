module Tmp where

import Data.Sequence
import Data.Semigroup
import Lang
import Control.Monad
import Control.Applicative
import Control.Monad.State

m :: (MonadPlus m, Applicative m, Semigroup a) => m a -> m a
m r = r `mplus` ((<>) <$> r <*> (m r))

type Reg' a b = StateT (Zip a) [] b

match2 :: (Eq a) => a -> b -> Reg' a b
match2 a b = do
  s <- get
  if a == (view' s) then put (forward s) >> return b else mzero

c :: (Semigroup b) => Reg' a b -> Reg' a b -> Reg' a b
c r r' = (<>) <$> r <*> r'

a :: Reg' a b -> Reg' a b -> Reg' a b
a = mplus

st :: Semigroup b => Reg' a b -> Reg' a b
st r = r `a` (r `c` st r)

st' r = foldl1 a [foldl1 c (Prelude.replicate n r) | n <- [1..]] 
