{-# LANGUAGE RankNTypes, ScopedTypeVariables, OverlappingInstances, DeriveFunctor #-}

module RegExAutomata where

import Prelude hiding (Either(..), either, (*))
import Control.Applicative
import Data.Monoid
import Machine
import Control.Arrow hiding (left, right)
import Control.Monad
import Mon

data List a = Nil | C a (List a) deriving (Eq, Ord, Show, Functor)

instance Monoid (List a) where
    mempty = Nil
    mappend Nil l = l
    mappend l Nil  = l
    mappend (C a l) l' = C a $ mappend l l'
    

type St = E (E () ())
    
type DualMonStack = Compose Dual St 

hom :: St b -> DualMonStack b
hom = Compose . Dual

acc :: b -> St b
acc t =  R t
n :: St a
n =  L $ R ()
err :: St a
err = L $ L ()


match :: (Eq a, Monoid b) => a -> b -> Moore a (St b) 
match c b = Moore (L $ R ()) $ \c' -> if c == c' then pure $ R b else pure $ L $ L () 

(<.>) :: (Monoid b) => forall a. Moore a b -> Moore a b -> Moore a b
m <.> m' = m >>= (mappend m' . pure)

(*) :: Monoid b => Moore a b -> Moore a b
(*) m = m <.> (*)m
          

toRegEx :: (Eq a, Monoid b) => [(a, b)] -> Moore a (St b)
toRegEx = foldl1 (<.>) . map (uncurry match) 

tag :: [b] -> [(b, List b)]
tag = map $ (fmap (flip C Nil) . join (,))
          

ifR = toRegEx $ tag "if"
forR = toRegEx $ tag "for"
openP = toRegEx $ tag "("
closeP = toRegEx $ tag ")"
spc = toRegEx $ tag " "

run :: Moore a (St b) -> [a] -> Moore a (St b)
run = foldl runMoore


