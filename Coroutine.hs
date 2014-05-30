module Coroutine where

import Control.Category
import qualified Prelude as P
import Control.Monad
import Control.Arrow


newtype Coroutine i o = Coroutine { runC :: i -> (o, Coroutine i o) }

instance Functor (Coroutine i) where
  fmap f (Coroutine g) = Coroutine P.$ \i -> let (o, co) = g i in (f o, fmap f co)


instance Category Coroutine where
  id = Coroutine (\i -> (i, id))
  cof' . cof = Coroutine func
   where
     func i = (o', c' . c)
       where
         (o, c) = runC cof i
         (o', c') = runC cof' o


instance Arrow Coroutine where
  
  arr f = Coroutine (\b -> (f b, arr f))

  first co =  Coroutine (\(b, d) -> let (c, co') = runC co b in ((c, d), first co'))
  


instance ArrowLoop Coroutine where
  
--  loop :: Coroutine (b,d) (c,d) -> Coroutine b c
  loop co = Coroutine (\b -> let ((c, d), co') = runC co (b, d) in (c, loop co')) 


{-
arr    :: (b -> c) -> a b c

(>>>)  :: a b c -> a c d -> a b d

first  :: a b c -> a (b,c) (c,d)

swap   :: (a,b) -> (b,a)

second :: a b c -> a (d,b) (d,c)

(***)  :: a b c -> a b' c' -> a (b, b') (c, c')

(&&&)  :: a b c -> a b d   -> a b (c, d)

clone a = (a,a)

(&&&) ar1 ar2 = arr clone >>> (ar1 *** ar2)

addA f g = f &&& g >>> arr (\(y,z) -> y + z)
addA f g = arr clone >>> 


-}