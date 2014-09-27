{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
--{-# LANGUAGE ExistentialQuantification #-}
--{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
--{-# LANGUAGE LambdaCase #-}

module Lens where

import Type
import Control.Monad.State

type Lens s t a b = Functor f => (a -> f b) ->  s -> f t

type Lens' s a = Lens s s a a

type ALens s t a b = LensLike (Pretext (->) a b) s t a b

lens :: (s -> a) -> (s -> b -> t) -> Lens s t a b
lens f g v s = fmap (g s) (v $ f s)

(%%~) :: Functor f => Lens s t a b -> (a -> f b) -> s -> f t
(%%~) f = f

(%%=) :: MonadState s m => Lens s s a b -> (a -> (r, b)) -> m r
(%%=) l f = case l f of
              sf -> do
                     s <- get
                     case sf s of
                       (r, s') -> do
                                 put s'
                                 return r
