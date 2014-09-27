{-# LANGUAGE RankNTypes #-}
module Getter where

import Control.Monad.Reader
import Control.Applicative
import Control.Monad.State

type Getter s a = forall r. (a -> Const r a) -> s -> Const r s

to :: (s -> a) -> Getter s a
to f f' s = case f' (f s) of
    Const r -> Const r

foldMapOf :: Getter s a -> (a -> r) -> s -> r
foldMapOf g f s = case g (Const . f) s of
                    Const r -> r

(^.) :: s -> Getter s a -> a
(^.) s g = foldMapOf g id s

view :: MonadReader s m => Getter s a -> m a
view g = do
  s <- ask
  return (s ^. g)

use :: MonadState s m => Getter s a -> m a
use g = do
  s <- get
  return (s ^. g)
