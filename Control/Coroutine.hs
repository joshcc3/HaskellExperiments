module Control.Coroutine where

import Control.Category
import qualified Prelude as P
import Control.Monad
import Control.Arrow
import Control.Applicative
import qualified Data.Bifunctor as B

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


swap (a,b) = (b,a)


instance ArrowChoice Coroutine where 
--  left :: Coroutine b c -> Coroutine (Either b d) (Either c d)
  left co = Coroutine f
    where
      f (P.Left b)  = B.bimap P.Left left (runC co b)
      f (P.Right d) = (P.Right d, left co)
      



{-

 mirror (Left x) = Right x
 mirror (Right x) = Left x

 right :: a b c -> a (Either d b) (Either d c)
 right co = arr mirror >>> left >>> arr mirror

 (+++) :: a b c -> a b' c' -> a (Either b b') (Either c c')
 (+++) co co' = left co >>> right co'

 (|||) :: a b d -> a c d -> a (Either b c) d
 (|||) co co' = co +++ co' >>> arr untag
    where
      untag (_ x) = x
-}
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