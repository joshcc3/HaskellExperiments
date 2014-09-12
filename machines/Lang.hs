{-# LANGUAGE  ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators  #-}
{-# LANGUAGE TypeFamilies  #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE  MultiParamTypeClasses #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE  DataKinds #-}
{-# LANGUAGE  GADTs #-}
--{-# LANGUAGE  #-}

import Prelude hiding ((+), (*))
import qualified Data.Foldable as F
import Control.Arrow 
import Data.Fix
import Control.Monad.Cont
import Machines.Mon
import Data.Monoid
import Machines.RegExAutomata hiding (match, (<|>), (<.>))
import Control.Monad
import Machines.Vec
import Machines.Prelude.Types

{-
We want to create a structure with the following interface.

<.> :: a -> a -> a
<|> :: a -> a -> a
fail :: a

Laws:
fail is the identity of <|>
assoc of <.> and <|>
fail <.> a = fail
a <.> fail = fail
distribtivity of <.> over <|>
This is provided by the monad plus interface
<.> = >>=
<|> = mplus
-}


match :: (MonadPlus m, Eq a, Monoid b) => a -> b -> m (a -> b)
match a b =  return (\a' -> if a == a' then b else mempty)

--bindF :: (MonadPlus m, Monoid b) => m (a -> b) -> (a -> b) -> m (a -> a -> b)
bindF m r = m >>= \r' -> return (\a a' -> r a <> r' a)

--(<.>) :: (MonadPlus m, Monoid b) =>m (a -> b) -> m (a -> b) -> m (a -> a -> b)
(<.>) r r' = r >>= bindF r'

--(<|>) :: (MonadPlus m, Monoid b) => m (a -> b) -> m (a -> b) -> m (a -> b)
(<|>) = mplus

class RegExp (n :: Nat) where
    toRegex :: (MonadPlus m, Monoid b, Eq a) => Vec(S  n) a -> b -> m (FunctorStream ((->) a) (S n) b)

--instance RegExp Z where
--    toRegex (Cons x _) b = match x b


--Vec (S (S n)) a -> b -> m (a -> a -> n -> b)

{-
instance RegExp n => RegExp (S n) where
    toRegex (Cons x v) b = a <.> b --undefined <.> undefined --toRegex v b
        where 
          a :: m (a -> b)
          a = undefined
          b = undefined
-}
--toRegex :: MonadPlus m => Vec n a -> b -> m (FunctorStream ((->) a) n b)
--toRegex v b = undefined -- F.foldl1 (<.>) (fmap (flip match mempty) $ initV v) <.> match (lastV v) b

--iff = toRegex (Cons 'i' $ Cons 'f' $ Cons 'f' Empty) (C "IF" Nil)



{-
So we can have two possible ways of doing this. 
We could build up the structure and then collapse it down
by in parallel calculating all the values. 
Alternatively we could use continuations to back track.
Either way we are going to use the F-Algebra to construct stuff
thus decoupling the construction from the evaluation. 
-}

{-
  Lets use continuations to represent backtracking.
  so back tracking arises from alternation. 
  We go down one branch. If that branch fails, then we want to backtrack 
  up to the last branch and then continue from that state. 
  The way this is traditionally implemented is by keeping a stack of states,
  pushing when you branch and popping when you fail.
-}

{-
Regular expressions machines will be coalgebras. 
The coalgebra will allow us to view the state.
-}

{-
Implementation 1:
We are going to run all the computations in parallel and gather the results eventually.
We will use lists for parallely running computations and either for sequential computations. 
These display the duality between construction and observation I think?
-}

--------------------------------------------------------------------------------

-- transducer: (Q, X, Y, d) - Q - set of states, X - inputs, Y - outputs, d - delta function
data CoalgF a b c = CoalgF (St b, a -> (St b, c)) deriving Functor



{-
So algebraic structures consist of some hidden state space. We can either observe some components of this space or we can step it forward. This transition from the state space to these observations or steppings is the functor or the co-algebra. The tutorial replaces functor with functions between sets. Product and sum types act as cartesian products and unions in this context. However the concept of a functor subsumes these because functors allow us to respect the more general notions of morphisms and composition between categories, which are implicit in functions. 

for a stream we have a functor that goes to the product type, of viewing the current state of the stream head and a function that progresses the stream to the next state which is tail.

Inductive definitions, define functions on the basis of the constructors that are used to construct the set.

Coinductive definitions define functions on the observable behaviour of the data.
Induction is an initial algebra while coinduction forms a final algebra.

A homormorphism f between an algebra F u -> u and F u' -> u' is a morhpism between the carrier 
sets u and u' that respects the functorial maps.
f . a = a' . T f.
A homorphism between algebras is a map between the carrier sets of the algebra that respects the properties of the functor

Another approach that seems to fit well with this using a coalgebra to represent stuff.
So we have an observable state. And we can step the internals of the machine as well.
So we need to provide the stepping function a -> (St b, c)
-}

{-
Monadplus interface apparently captures exactly what I'm trying to get
-}
