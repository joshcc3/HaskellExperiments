{-

Going to try and implement breadth first search using CPS
The algorithm is to visit each depth 
ans i = (vs, cs)
ans (i+1) = vs + vs', 
-}

loeb :: Functor f => f (f a -> a) -> f a
loeb f = let x = fmap ($ x) f in x

moeb :: (((a -> b) -> b) -> c -> a) -> c -> a
moeb f c = let x = f (\g -> g x) c in x

loeb' = moeb fmap
