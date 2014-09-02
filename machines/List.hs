module List where


data List a x = List a x | Nil

(|>) x l = List x l

instance Functor (List a) where
    fmap f (List a x) = List a (f x)
    fmap f Nil = Nil



list = 1 |> 2 |> 'c' |> Nil