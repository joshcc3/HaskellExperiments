{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Tmp where

import Prelude hiding ((.), id)
import Control.Category
import Control.Arrow
import Control.Lens
import Data.Bifunctor
import Control.Monad
import Pipes
import Control.Applicative

distrIso :: Iso (a -> (b, c)) (d -> (e, f)) (a -> b, a -> c) (d -> e, d -> f)
distrIso = iso (\f -> (\a -> fst $ f a, \a -> snd $ f a)) (\(f, f') a -> (f a, f' a))

dot :: (b -> c) -> (a -> b) -> a -> c
dot f g x = f (g x)

newtype Co' m a b = Co { runCo :: a -> m (b, Co' m a b) }

type Co = Co' Identity

instance (Monad m, Functor m) => Category (Co' m) where
    Co f . Co g = Co $ \a -> let m = g a in m >>= \(b, c) -> (fmap . fmap) (. c) (f b)
    id = Co $ \a -> return (a, id)

instance (Monad m, Functor m) => Arrow (Co' m) where
    arr f = Co $ \a -> return (f a, arr f)
    first (Co f) = Co $ \(a, c) -> f a >>= \(b, c') -> return ((b, c), Control.Arrow.first c')

instance ArrowLoop (Co' Identity) where
    loop (Co f) = Co g
        where 
          g b = do
            let Identity ((c, d), c') = f (b, d) 
            return (c, loop c')

instance Monad m => Monad (Co' m a) where
    return a = Co $ const $ return (a, return a)
    (Co f) >>= g = Co $ \a -> f a >>= \(b, c') -> runCo (g b) a >>= \(c, _) -> return (c, c' >>= g)

instance (Monad m, Functor m) => Functor (Co' m a) where
    fmap f (Co g) = Co $ fmap (bimap f (fmap f)) . g

instance (Monad m, Applicative m) => Applicative (Co' m a) where
    pure = return
    c <*> c'  = do
      f <- c
      a <- c'
      return (f a)
      
                  

withPrevious :: Monad m => a -> Co' m a (a, a)
withPrevious a = Co $ \a' -> return ((a, a'), withPrevious a')

delay :: (Monad m, Functor m) => a -> Co' m a a
delay a = withPrevious a >>> arr fst

sumToN :: Co Int Int
sumToN = loop $ id *** delay 0 >>> arr (\(b, i) -> (b+i, i+1))

close :: Co a a -> a -> Co () a
close x a = loop $ Control.Arrow.second (delay a) >>> Control.Arrow.second x >>> arr (join (,) . snd)

data Void

fastForward :: (Functor m, Monad m) => Co' m a b -> Producer a m r -> Co' m Void b
fastForward c p = Co $ \_ -> next p >>= \case
                               Left _ -> error "Broken Stream"
                               Right (a, p') -> (fmap . fmap) (flip fastForward p') $ runCo c a

replaceWithMax :: [Int] -> [Int]
replaceWithMax l = ans
 where
  (ans, maximum) = f (l, head l)
  f (l:ls, m) = Control.Arrow.first (maximum:) $ f (ls, max m l)
  f x = x

{-
int a = 0;
for (int i = 0; i < 10; i++){
   a += i;  
}
Co $ \(a, i) -> (a + i, i + 1)
(a+i, 
((a + i, i + 1), c')
-}
(<**>) :: (a -> b, c -> d) -> (a, c) -> (b, d)
(<**>) = uncurry (***)
