{-# LANGUAGE RankNTypes, ScopedTypeVariables, OverlappingInstances, DeriveFunctor, GADTs, PolyKinds #-}

module RegExAutomata where

import Prelude hiding (Either(..), either, (*))
import Control.Applicative hiding ((<|>))
import Data.Monoid hiding (Last, getLast)
import qualified Control.Category as C
import Control.Arrow
import qualified Data.Machine as M
import Control.Arrow hiding (left, right)
import Control.Monad
import Mon
import Data.Sequence

data Tok = IF | FOR | OPEN_P | CLOSE_P | INT_LIT Int | SEMI_COLON | VAR Char | EQUALS

data List a = Nil | C a (List a) deriving (Eq, Ord, Show, Functor)

{-
F(f) . F(g) = F(f * g) 

-}


    


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

type St = E (E () ())

hom :: E (E a b) b1 -> E b1 (E b a)
hom = fmap iso . iso 

hom' :: E b1 (E b a) -> E (E a b) b1
hom' = iso . fmap iso 

g :: E (E b a1) a -> E (E b a1) a
g = iso . fmap iso . fmap iso . iso

acc :: b -> St b
acc t =  R t
n :: St a
n =  L $ R ()
err :: St a
err = L $ L ()


dot = M.Mealy $ \a -> (acc (C a Nil), pure $ acc (C a Nil))

match :: (Eq a, Monoid b) => a -> b -> M.Mealy a (St b)
match c b = M.Mealy $ \a -> if a == c then (acc b, pure $ acc b) else (err, pure err)
-- (base <.> (h (g base <> g base ))
(<.>) :: Monoid b => forall a. M.Mealy a (St b) -> M.Mealy a (St b) -> M.Mealy a (St b)
(<.>) m m' = C.id &&& m >>> M.unfoldMealy f (m', False)
    where 
      f s (_, L(L ())) = (err, s)
      f s (_, L(R ())) = (n, s)
      f s@(machine, flag) (inp, a) = if flag then (a <> (fst $ M.runMealy machine inp), (snd $ M.runMealy machine inp, flag)) else (n, (machine, True))


(<|>) :: Monoid b => M.Mealy a (St b) -> M.Mealy a (St b) -> M.Mealy a (St b)
(<|>) m m' =  undefined -- m <> fmap 
--    where 
--      homomorph = fmap (Compose . pure)



(*) :: Monoid b => M.Mealy a (St b) -> M.Mealy a (St b)
(*) m = undefined -- m <> (m <.> (*)m)
--(*) m = fmap hom' (fmap hom m <> fmap hom (m <.> (*)m))


{-

(<.>) :: (Monoid b) => forall a. Moore a (St b) -> Moore a (St b) -> Moore a (St b)
m <.> m' = m >>= f
    where
        f = g . either (either errorHandler nHandler) acceptHandler
        errorHandler _ = pure err <> m'
        acceptHandler b = pure (acc b) <> m'
        nHandler _ = pure n <> Moore mempty (const m')
        g :: E (E (Moore a b) (Moore a b)) (Moore a b) -> Moore a b
        g (R x) = x
        g (L (L x)) = x
        g (L (R x)) = x

match :: (Eq a, Monoid b) => a -> b -> Moore a (St b) 
match c b = Moore n $ \c' -> if c == c' then pure $ acc b else pure err 

-}

instance Monoid b => Monoid (M.Mealy a b) where
    mempty = pure mempty
    mappend = liftA2 mappend


toRegEx :: (Eq a, Monoid b) => [(a, b)] -> M.Mealy a (St b)
toRegEx = foldl1 (<.>) . map (uncurry match) 

tag :: [b] -> [(b, List b)]
tag = map $ (fmap (flip C Nil) . join (,))


ifR = toRegEx $ tag "if"
forR = toRegEx $ tag "for"
openP = toRegEx $ tag "("
closeP = toRegEx $ tag ")"
spc = toRegEx $ tag " "


run :: M.Mealy a1 a -> [a1] -> a1 -> a
run m [] final = fst $ M.runMealy m final
run m (l:ls) final = run (snd $ M.runMealy m l) ls final


