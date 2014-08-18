import Prelude hiding ((^))
-- | (Church Encoding?) of sets
data U = U { f :: U -> Bool }

emptySet = U $ const False

-- | Set membership, checks if b is a member of a
(|>) :: U -> U -> Bool
b |> a = f a b


-- | Power of functions f ^ n ~ f(f(...()...) n times
ƒ ^ 0 = id
ƒ ^ n = ƒ . (ƒ ^ (n - 1))

-- | µ a is the set of all sets that contain a as a member
µ a = U $ \s -> a |> s

-- | phi 0 - emptyset, phi 1 - set of all sets containing the empty set, 
-- | phi 2 - set of all sets that contain sets containing the empty set
-- | these sets are different from each other and are strictly larger
phi n = µ ^ n $ emptySet


-- | The set of all sets that do not contain themselves
notContainingSelf = U $ \a ->not $ a |> a

russelsParadox = notContainingSelf |> notContainingSelf














{-# LANGUAGE RankNTypes, FlexibleInstances, FunctionalDependencies, MultiParamTypeClasses, ScopedTypeVariables #-}

module Main where
    
import Prelude hiding ((.), id, (*))
import Control.Applicative
import Data.Monoid


data St = Err | N | A deriving (Eq, Show, Ord, Bounded)


instance Monoid St where
    mempty = minBound
    mappend = max

data StT a = ErrT a | NT a | AT a deriving (Eq, Show, Ord)

{-


M x N -> O

O { unit, mappend }

M:m N:n O:o -> f : m x n -> o
M:m' N:n' O:o' -> m' x n' -> o'

o <> mempty = o
mempty <> o = o
o <> (o' <> o'') = (o <> o') <> o''

f(m x n) <> mempty = f(m x n)
mempty <> f(m x n) = f(m x n)
f(m x n) <> (f(m' x n') <> f(m'' x n'')) = (f(m x n) <> f(m' x n')) <> f(m'' x n'')

Are the above laws sufficient to adequately describe the notion presented by StT [a].
what that succeeds in doing is allowing the outer monoid to perform its

-}

class (Monoid a, Monoid b) => MonHomo a b c | a b -> c where
    hom :: a -> b -> c

instance MonHomo St [a] (StT [a]) where
    hom A l = AT l
    hom Err l = ErrT l
    hom N l = NT l

instance Monoid (StT [a]) where
    mempty = ErrT (mempty)
    mappend (AT a) (AT b) = AT $ a <> b
    mappend (AT a) _ = AT $ a 
    mappend _ (AT a) = AT $ a
    mappend (ErrT a) (ErrT b) = ErrT $ a <> b
    mappend (NT a) (NT b) = NT $ a <> b
    mappend (NT a) _ = NT a
    mappend _ (NT a) = NT a


    
