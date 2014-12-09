{-# LANGUAGE TupleSections #-}
module Test where

import Control.Lens
import Data.Bifunctor

mToE :: Iso' (Either a ()) (Maybe a)
mToE = iso f g
    where 
      f (Left a) = Just a
      f (Right ()) = Nothing
      g Nothing = Right ()
      g (Just a) = Left a

iso1 :: Iso' ((a, Either (b->r) ()) -> r) (a -> r) 
iso1 = iso (\f a -> f (a, Right ())) (\f (a, _) -> f a)

iso2 :: Iso' (a, Either b c) (Either (a, b) (a, c))
iso2 = iso (\(a, e) -> bimap (a,) (a,) e) g
    where 
      g (Left (a, b)) = (a, Left b)
      g (Right (a, c)) = (a, Right c)

iso3 :: Iso' (a, ()) a
iso3 = iso (uncurry const) (,())

iso4 :: Iso' (a, (b, c)) ((a, b), c)
iso4 = iso (\(a, (b, c)) -> ((a, b) , c)) (\((a, b), c) -> (a, (b, c)))

-- prism1 = 
