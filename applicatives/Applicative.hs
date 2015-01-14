{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import Control.Applicative hiding (Writer, Const)
import Data.Monoid

data Compose f g a = Compose { getCompose :: f (g a) }

data Const a r = Const a

data Identity a = Id a

instance (Functor f, Functor g) => Functor (Compose f g) where
  fmap f (Compose c) = Compose $ (fmap . fmap) f c

instance (Applicative f, Applicative g) => Applicative (Compose f g) where
  pure a = Compose $ pure (pure a)
  Compose f <*> Compose g =  Compose $ (<*>) <$> f <*> g

instance Functor Identity where
  fmap f (Id a) = Id $ f a

instance Applicative Identity where
  pure a = Id a
  Id f <*> Id a = Id $ f a

instance Functor (Const a) where
  fmap f (Const a) = Const a

instance Monoid a => Applicative (Const a) where
  pure a = Const mempty
  Const a <*> Const b = Const (a <> b)


newtype Prod f g a = Prod (f a, g a) 

instance (Functor f, Functor g) => Functor (Prod f g) where
  fmap f (Prod (a, b)) = Prod (fmap f a, fmap f b)

instance (Applicative f, Applicative g) => Applicative (Prod f g) where
  pure a = Prod (pure a, pure a)
  Prod (f, f') <*> Prod (a, a') = Prod (f <*> a, f' <*> a')


newtype Writer w a = Writer { getWriter :: Prod (Const w) Identity a } deriving (Functor, Applicative)

type State s a = Compose ((->) s) (Writer s) a


tell :: (s -> s') -> Writer s a -> Writer s' a
tell f (Writer (Prod (Const a, b))) = Writer (Prod (Const (f a), b))

read :: Writer s a -> s
read (Writer (Prod (Const a, b))) = a

ask :: s -> s
ask = id

get :: Monoid s => State s s
get = Compose pure

--put :: Monoid s => s -> State s ()
--put s = Compose $ fmap (tell $ const s) (tell (const s) (pure ()))




{-
push :: Int -> State [Int] () 
push v = (v:) <$> 

pop :: State [Int] Int
-}
