module DiffLists where


type DiffList a = [a] -> [a]

empty :: DiffList a
empty = id


(|>) :: a -> DiffList a -> DiffList a
(|>) a d = (a:) . d


(<|) :: DiffList a -> a -> DiffList a
(<|) d a = d . (a:)
