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