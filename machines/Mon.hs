{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, PolyKinds, GADTs #-}

module Mon where

import Data.Bifunctor
import Control.Applicative
import Data.Monoid
import Control.Monad
import Control.Arrow 

data E a b = L a | R b deriving (Eq, Ord)

left :: E a b -> Maybe a
left (L a)  = Just a
left _ = Nothing

right :: E a b -> Maybe b
right (R a)  = Just a
right _ = Nothing

isoE (L x) = Left x
isoE (R x) = Right x

isoE' (Left x) = L x
isoE' (Right x) = R x

distributes :: (t1, Either t t2) -> Either (t1, t) (t1, t2)
distributes (a, Left x) = Left (a, x)
distributes (a, Right x) = Right (a, x)

distributes' ::  Either (a, b) (a, c)-> (a, Either b c)
distributes' (Left (a, x)) = (a, Left x)
distributes' (Right (a, x)) = (a, Right x)

instance Bifunctor E where
    bimap l r (L x) = L $ l x
    bimap l r (R x) = R $ r x

instance Functor (E a) where
    fmap f (L a )  = L a
    fmap f (R b) = R $ f b

instance Monoid a => Applicative (E a) where
    pure x = R x
    (R x) <*> (R y) = R $ x y
    (L x) <*>  (L y) = L (x <> y)
    (L x) <*> _ =  L x
    _ <*> (L x) = L x


iso :: E a b -> E b a
iso (L x) = R x
iso (R x) = L x



either :: (a -> b) -> (c -> d) -> E a c -> E b d
either f _ (L a) = L $ f a
either _ g (R b) = R $ g b

    
newtype Compose f g a = Compose { unwrap :: f (g a) }
newtype Prod f g a = Prod { getProd :: (f a, g a) }

instance (Functor f, Functor g) => Functor (Prod f g) where
    fmap f p = Prod $ (fmap f *** fmap f) (getProd p)

instance (Applicative f, Applicative g) => Applicative (Prod f g) where
    pure x = Prod $ (pure x, pure x)
   -- original (Prod (f, g)) <*> (Prod (f', g')) = Prod $ (f <*> f', g <*> g')
    (<*>) = (.) Prod . ((flip (.) getProd) . (uncurry (***) . ((<*>) *** (<*>)))) . getProd
    
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

