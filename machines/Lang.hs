{-# LANGUAGE  ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators  #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE  MultiParamTypeClasses #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE Rank2Types #-}
--{-# LANGUAGE  #-}

import Prelude hiding ((+), (*))
import Control.Arrow 
import Data.Fix
import Control.Monad.Cont
import Mon
import Data.Monoid
import RegExAutomata hiding (match)
import Control.Monad

data MyList a = a :- MyList a | Null

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

type Regex a b r =  (St b, [a]) -> Cont r (St b, [a])

toReg b l = foldl1 conc $ map (match mempty) (init l) ++ [match b (last l)]

type MStack = IO

ifReg :: (St (List [Char]), [Char]) -> MStack (St (List String), String)
ifReg = star $ toReg (C "IF" Nil) "if"

match _ _ (c', []) = return (err, [])
match b a (c', s) = return $ (if a == head s then( acc b <> c', tail s) else (err, tail s))

conc :: Monad m => (a -> m b) -> (b -> m c) -> a -> m c
conc = (>=>) 

alter r r' = r''
    where 
      r'' inter@(b, s) = r inter >>= \(b', s') -> case b' of
                                                    L (L x) -> r' inter
                                                    z -> return (b', s')
star r = r `alter` (r `conc` star r)

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

