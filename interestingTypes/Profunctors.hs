class Profunctor p where
    lmap :: (a -> b) -> p b c -> p a c
    rmap :: (a -> b) -> p c a -> p c b


instance Profunctor (->) where
    lmap f g = g . f
    rmap f g = f . g


data UpStar f b c = UpStar { getU :: b -> f c }

instance Functor f => Profunctor (UpStar f) where
    lmap f g = UpStar $ getU g . f
    rmap f g = UpStar $ (fmap . fmap) f (getU g)


data DownStar f b c = DownStar { getD :: f b -> c }

instance Functor f => Profunctor (DownStar f) where
    lmap f g = DownStar $ getD g . fmap f
    rmap f g = DownStar $ f . getD g
