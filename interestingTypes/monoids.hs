{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}


import Control.Applicative
import Data.Monoid

data E a b = L a | R b deriving (Eq, Ord, Show)

instance Functor (E a) where
    fmap f (L a )  = L a
    fmap f (R b) = R $ f b


instance Monoid a => Applicative (E a) where
    pure x = R x
    (R x) <*> (R y) = R $ x y
    (L x) <*>  (L y) = L (x <> y)
    (L x) <*> _ =  L x
    _ <*> (L x) = L x

{-
instance (Monoid a, Monoid b) => Monoid (E a b) where
    mempty = R mempty
    mappend = liftA2 (mappend)
-}
-- so to the string monoid we want to add the capability of string accumulation

newtype Compose f g a = Compose { unwrap :: f (g a) }

instance (Functor f, Functor g) => Functor (Compose f g) where
    fmap f (Compose c) = Compose $ (fmap . fmap) f c

instance (Applicative f, Applicative g) => Applicative (Compose f g) where
    pure x = Compose $ (pure . pure) x
    (Compose c) <*> (Compose c') = Compose $ (fmap (<*>) c) <*> c'

newtype Identity a = Identity {getId :: a}

instance Functor Identity where
    fmap f i = Identity $ f $ getId i

instance Applicative Identity where
    pure x = Identity x
    (Identity f) <*> (Identity a) = Identity $ f a

instance (Applicative f, Monoid a) => Monoid (f a) where
    mempty = pure mempty
    mappend = liftA2 mappend

type St a = E (E () ()) a
    
type MonStack = Compose Identity (E (E () ())) ()

a :: St ()
a = L $ L ()
n :: St ()
n = L $ R ()
e :: St ()
e = R ()

st1 = Identity $ a
st2 = Identity $ n
st3 = Identity $ e

main = do
    print $ getId $ st2 <> st3
