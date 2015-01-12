{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE DeriveFunctor #-}
module Tmp where

import Control.Monad
import qualified Data.Map as M
import Prelude hiding ((.), id)
import Control.Category
import LambdaTerms
import LambdaCalc
import Control.Applicative

data CType a = TVar a | CType a :-> CType a deriving (Eq, Ord, Show, Functor)

instance Applicative CType where
    pure a = pure a :-> pure a
    TVar f <*> TVar a = TVar (f a)
    c <*> (a :-> b) = (c <*> a) :-> (c <*> b)
    (a :-> b) <*> c = (a <*> c) :-> (b <*> c)

instance Monad CType where
    return = pure

    (TVar a) >>= f = f a
    (a :-> b) >>= f = (a >>= f) :-> (b >>= f)

type Subst a = a -> CType a

-- substitutions should always be total functions

-- contexts should necessarily be total functions


data Arr a b = forall m. Monad m => Arr (a -> m b) (M.Map a (m b))

instance Category Arr where
    id = Arr (return :: a -> CType a) M.empty 
    Arr f m . Arr f' m' = 

{-
So substitution 
-}

{-
The identity law:
pure id <*> v = v
Homomorphism:
pure f <*> pure x = pure (f x)
Intuitively, applying a non-effectful function to a non-effectful argument in an effectful context is the same as just applying the function to the argument and then injecting the result into the context with pure.
Interchange:
u <*> pure y = pure ($ y) <*> u
Intuitively, this says that when evaluating the application of an effectful function to a pure argument, the order in which we evaluate the function and its argument doesn't matter.
Composition:
u <*> (v <*> w) = pure (.) <*> u <*> v <*> w
This one is the trickiest law to gain intuition for. In some sense it is expressing a sort of associativity property of (<*>). The reader may wish to simply convince themselves that this law is type-correct.

-}
