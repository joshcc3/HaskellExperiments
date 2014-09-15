{-# LANGUAGE TypeSynonymInstances, TupleSections, FlexibleInstances, ScopedTypeVariables, OverlappingInstances, DeriveFunctor, DataKinds, FlexibleContexts, NoMonomorphismRestriction #-}

module RegExAutomata where

import qualified Data.Foldable as F
import Vec
import Prelude hiding (either, (*))
import Control.Applicative hiding ((<|>))
import Data.Monoid hiding (Last, getLast)
import qualified Control.Category as C
import Control.Arrow hiding ((|||))
import qualified Data.Machine as M
import Control.Arrow hiding (left, right)
import Control.Monad
import Mon
import Data.Sequence
import qualified Data.Bifunctor as B

data Tok = IF | FOR | OPEN_P | CLOSE_P | INT_LIT Int | SEMI_COLON | VAR Char | EQUALS | SPC deriving (Eq, Ord, Show)

data List a = Nil | C a (List a) deriving (Eq, Ord, Show, Functor)

instance Monoid (List a) where
    mempty = Nil
    mappend Nil l = l
    mappend l Nil  = l
    mappend (C a l) l' = C a $ mappend l l'
    
    
iso' :: List a ->[a]
iso' Nil = []
iso' (C a l) = a : iso' l

iso'' :: [a] -> List a
iso'' [] = Nil
iso'' (a : as) = C a (iso'' as)

-- our state is Nothing | Err | N | Acc x
type St = E (E (E () ()) ())

instance Show b => Show (St b) where
    show (L (L (L ()))) = "Nothing"
    show (L (L (R ()))) = "Error"
    show (L (R ())) = "Not Accepting"
    show (R b) = "Accepting " ++ show b

hom = (fmap . fmap) iso . fmap iso . iso . fmap Last

-- fmap f . fmap g = fmap (f . g)
-- fmap (f . g) = fmap f . fmap g
-- fmap (fmap iso) . fmap iso . iso
-- (fmap . fmap) iso . fmap iso . iso
-- hom x = fmap^n x <> fmap^(n-1) x <> ... fmap^(0) x
-- hom x = iso + fmap . hom
-- x = 1 + y*x
-- (1 - y)*x = 1
-- x = 1/(1 - y) 
-- hom = iso/(iso - fmap)
-- negative and fractional types?

hom' = fmap getLast . iso . fmap iso . (fmap . fmap) iso

stIso :: E (E (E () ()) ()) b -> Either (Either (Either () ()) ()) b
stIso = (B.first . B.first) isoE . B.first isoE . isoE

stIso' :: Either (Either (Either a b) c1) c -> E (E (E a b) c1) c
stIso' = (B.first . B.first) isoE' . B.first isoE' . isoE'

acc :: b -> St b
acc t =  R t
n :: St a
n =  L $ R ()
err :: St a
err = L $ L $ R ()
nothing = L (L (L ()))

dot b = M.Mealy $ \a -> (acc (C b Nil), pure (acc (C b Nil)))

match :: (Eq a, Monoid b) => a -> b -> M.Mealy a (St b)
match c b = M.Mealy $ \a -> if a == c then (acc b, pure $ acc b) else (err, pure err)

conc :: Monoid b => forall a. M.Mealy a (St b) -> M.Mealy a (St b) -> M.Mealy a (St b)
conc m m' =     C.id &&& (m >>> arr isoE) 
             >>> arr distributes 
             >>> arr (L . snd) ||| machine
    where 
--      machine :: M.Mealy (a, b) (St b)
      machine =    (dropMealy 1 n >>> (C.id ||| m')) *** arr R 
                >>> arr ( uncurry (flip (<>)))

dropMealy :: Int -> d -> M.Mealy a (Either d a)
dropMealy n d = M.unfoldMealy (\s a -> if s > 0 then (Left d, s - 1) else (Right a, s)) n

(<.>) m m' = m >>= (flip fmap m' . conc)

(<|>) :: MonadPlus m => m (M.Mealy a b) -> m (M.Mealy a b) -> m (M.Mealy a b)
(<|>) = mplus

instance Monoid b => Monoid (M.Mealy a b) where
    mempty = pure mempty
    mappend = liftA2 mappend



collapse l = fmap hom' $ F.fold $ (fmap . fmap) hom l

(*) m = m <|> (m <.> (*)m)


toRegEx r = foldl1 (<.>) . map (return . uncurry match) $ r


tag = map $ (fmap (flip C Nil) . join (,))

tag' :: Monoid b => [a] -> b -> [(a, b)]
tag' l b = map (,mempty) (init l) ++ [(last l, b)]

ifR = toRegEx $ tag' "if" (C IF Nil)
forR = toRegEx $ tag' "for" (C FOR Nil)
openP = toRegEx $ tag' "(" (C OPEN_P Nil)
closeP = toRegEx $ tag' ")" (C CLOSE_P Nil)
spc = toRegEx $ tag' " " (C SPC Nil)


forward :: M.Mealy a1 a -> [a1] -> a1 -> (a, M.Mealy a1 a)
forward m [] final = M.runMealy m final
forward m (l:ls) final = forward (snd $ M.runMealy m l) ls final

