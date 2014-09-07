{-# LANGUAGE  ScopedTypeVariables #-}
{-# LANGUAGE  TypeFamilies #-}
--{-# LANGUAGE  #-}

import Data.Fix
import Mon

data LangF a n
 = Stop | Br n n


instance Functor (LangF a) where

data Stat = Print String | Read String

{-
For the F-Algebra make the carrier set continuations.
We want to build up continuations for the backtracking 
computations and we will use an F-Algebra to achieve that.
-}

newtype Cont r a = Cont { runCont :: (a -> r) -> r }
