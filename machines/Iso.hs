{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}


module Iso where

import Machines.Mon (E(..))
import Machines.RegExAutomata (List(..))
import Machines.Vec 

class Iso a b where
    iso :: a -> b
    iso' :: b -> a

instance Iso (E a b) (Either a b) where
    iso (L x) = Left x
    iso (R x) = Right x

    iso' (Left x) = L x
    iso' (Right x) = R x

instance Iso (E a b) (E b a) where
    iso (L x) = R x
    iso (R x) = L x

    iso' = iso

instance Iso (List a) [a] where
    iso Nil = []
    iso (C a l) = a : iso l

    iso' [] = Nil
    iso' (a : as) = C a (iso' as)


class Hom a b where
    to :: a -> b

instance Hom (Vec Z a -> b) b where
    to f = f Empty               



{-

Try and create a type family for an isomorphism
between a stream of Es and a stream to eithers.

-}