{-# LANGUAGE DataKinds, PolyKinds, UndecidableInstances, GADTs, KindSignatures, TypeFamilies #-}
  
import Data.Foldable
import Control.Applicative
import Data.Monoid

data S n = S n
data Z = Z

--Add x (Add y z) ~ Add (Add x y) z

{-
so what we want to do is prove that (Add x (Add y z)) has the same type as (Add (Add x y) z)
The only inhabitant of the same type is Refl.
-}

data Iso a b where
    Refl :: Iso a a

sym :: Iso a b -> Iso b a
sym Refl = Refl


class Assoc x where
    assoc :: x -> y -> z ->  Iso (Add x (Add y z)) (Add (Add x y) z)

instance Assoc Z where
    assoc Z _ _ = Refl


instance Assoc n => Assoc (S n) where
--    assoc :: Iso (Add (S n) (Add y z)) (Add (Add (S n) y) z)
    assoc (S n) y z = cong $ assoc n y z


cong :: Iso a b -> Iso (S a) (S b)
cong Refl = Refl


type family Add a b
type instance Add Z n = n
type instance Add (S n) n' = S (Add n n')

--type family Mul a b
--type instance Mul Z n = Z
--type instance Mul (S n) n' = Add n' (Mul n n')

