{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, RankNTypes, ScopedTypeVariables, OverlappingInstances, DeriveFunctor, GADTs, PolyKinds #-}

module RegExAutomata where

import Prelude hiding (either, (*))
import Control.Applicative hiding ((<|>))
import Data.Monoid 
import qualified Control.Category as C
import Control.Arrow hiding ((|||))
import qualified Data.Machine as M
import Control.Arrow hiding (left, right)
import Control.Monad
import Mon
import Data.Sequence
import qualified Data.Bifunctor as B

data Tok = IF | FOR | OPEN_P | CLOSE_P | INT_LIT Int | SEMI_COLON | VAR Char | EQUALS

data List a = Nil | C a (List a) deriving (Eq, Ord, Show, Functor)

{-


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

-- our state is Nothing | Err | N | Acc x
type St = E (E (E () ()) ())

instance Show b => Show (St b) where
    show (L (L (L ()))) = "Nothing"
    show (L (L (R ()))) = "Error"
    show (L (R ())) = "Not Accepting"
    show (R b) = "Accepting " ++ show b

hom = (fmap . fmap) iso . fmap iso . iso

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

hom' = iso . fmap iso . (fmap . fmap) iso

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
-- (base <.> (h (g base <> g base))
(<.>) :: Monoid b => forall a. M.Mealy a (St b) -> M.Mealy a (St b) -> M.Mealy a (St b)
(<.>) m m' =     C.id &&& (m >>> arr isoE) 
             >>> arr distributes 
             >>> arr (L . snd) ||| machine
    where 
--      machine :: M.Mealy (a, b) (St b)
      machine =    (dropMealy 1 mempty >>> (C.id ||| m')) *** arr R 
                >>> arr (uncurry (<>))

dropMealy :: Int -> d -> M.Mealy a (Either d a)
dropMealy n d = M.unfoldMealy (\s a -> if s > 0 then (Left d, s - 1) else (Right a, s)) n

{-
{-
Right so we need to redefine <.> (*) and <+> (+) so that <.> distributes over <+>.
We need to redefine the plus.
So the answer should be expressed as the result of logging the machine. 
-}
(<|>) :: Monoid b => M.Mealy a (St b) -> M.Mealy a (St b) -> M.Mealy a (St b)
(<|>) m m' =  h $ g m <> g m'
    where 
      g = fmap hom
      h = fmap hom'

(*) :: Monoid b => M.Mealy a (St b) -> M.Mealy a (St b)
(*) m = m <> (m <.> (*)m)
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
