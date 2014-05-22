{- i have my units. They all have strategies for moving. The transformations
I want to make on the units are, combine them with other units,
combine teams of units together. A unit is defined by the way it moves.

the idea is that we have a unit. That unit is defined by its strategy
for movement (shall define this strategy in a bit). We want to combine
these units together. Combining these units together means that theyare
 composed together. Each acts on the output of the other. That is what
 we mean by interaction. They act on the basis of the output or actions or
 the function of the other that is what we mean by interaction. The data type
 must be the morphisms. We want the interaction category. That is, this is
the category where the morphisms relate two objects, one that has a strategy
a, the other has a strategy b. The resultant morphism produces a composition
of these strategies. We cannot combine any two strategies. Only strategies
that fit together - What is a strategy. We can think of a strategy as an AST
 in the DSL. What are the strategies that fit to gether?

Example strategy keep moving forward. Another strategy keep moving backward.

One way of thinking to combine strategies is that we leave holes in the AST.
 The holes are gaps to be filled in by the output of the dependency. Thus
allowing us to combine strategies.

Ok lets get this straight. We have a category. Lets call this category Strategy.
The Strategy category has objects which are strategies. morphisms, combine strategies.

Strategies will be expressed as an AST with a provision for the type of the hole in it.
A strategy 'a' and a strategy 'b' are compatible if their holes are compatible.

-}

{-# LANGUAGE DeriveFunctor #-}

import qualified Control.Category as C
import qualified Control.Monad as M

data Free f r = Free (f (Free f r)) | Pure r

instance Functor f => Monad (Free f) where

   return = Pure

   (>>=) (Pure a) f    = f a
   (>>=) (Free f) func = Free (fmap (>>= func) f)


liftF :: Functor f => f r -> Free f r
liftF = Free . (fmap Pure)

data Instruction next a = Front next | Back next | RotateR Int next |
                          RotateL Int next | Hole a next | Done deriving (Functor)

frnt     = liftF (Front ())
back     = liftF (Back ())
rotR x   = liftF (RotateR x ())
rotL x   = liftF (RotateL x ())
hole a   = liftF (Hole a)


strat1 = do
  M.replicateM_ 10 frnt
  rotR 180
  M.replicateM_ 10 back
  rotR 180
  strat1


strat2 = do
  rotR 5
  strat2

{-
An example of a combinable strategy is for example - one bot wants to
consistently follow another bot. Therefore the hole in the bot that wants
to follow the other bot should allow for any strategy to fill that hole.
What does composing these strategies mean. Composing the strategies means
that-}
