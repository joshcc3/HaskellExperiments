> {-# LANGUAGE DeriveFunctor #-}

he last installment introduced the notion of comonads as a dual to monads.
We also developed the intuition that comonads allow us to extract results from values inside a context.
This episode we're going develop an engine for conways game of life.
The actions of each individual unit is dependent on its environment - a context.

Before we go any further we're going to develop another interesting comonadic structure, a zipper over a stream.
The zipper has as its focus some element and extends infintely in both directions. Thus our zipper consists of a
stream that extends infinitely in the left direction, the current focus and a list that extends infinitely in the
right direction.

> import Control.Comonad

> data LZip a = LZip [a] a [a] deriving (Eq, Ord, Show, Functor)

We want to be able to move around in the structure for which we develop two utility functions,

> shiftL :: LZip a -> LZip a
> shiftL (LZip l a r) = LZip (tail l) (head l) (a:r)

This moves the focus to the left pushing the current value to the left.

> shiftR :: LZip a -> LZip a
> shiftR (LZip l a r) = LZip (a:l) (head r) (tail r)

This moves the focus to the right pushing the current value to the left.

So this zipper has an element it focuses on, and has a context (its position in the stream).
That sounds pretty much like the requirement for a comonad and we can indeed derive a comonadic
instance for it.

> instance Comonad LZip where
>    extract (LZip _ a _) = a

Extract gives us the current focus in the zipper.

Duplicate has to give us a LZip (LZip a). That structure gives us a stream of parallel streams.
After duplicate the structure that we obtain is such that, the stream that is focused on as we move
to the right is the original stream shifted to the right and vice versa for the left.

>    duplicate z@(LZip l v r)
>        = LZip left z right
>          where
>            left = tail $ iterate shiftL z
>            right = tail $ iterate shiftR z

Conways game of life consists of an environment with synchronous development happening across
all of the cells. The status of the cell is decided by its current status and its environment.
A cells status is simply that of dead or alive for which we will use booleans.

That's a function of the form:

step :: Env Bool -> Bool

The rules are as follows:


We need to be able to step to the left, right, and up and down.
This is where the list zipper comes into play. A duplicated zipper will represent the environment of
a particular process.

> type Env a = LZip (LZip a)

Finally deciding whether or not a cell stays alive till the next iteration is as simply as
observing the status of the surroundings cells and applying the rules.

step :: Conway -> Bool
rstep z  | count < 2 = False
         | count == 3 = True
         | alive && count == 2 = True
         | otherwise = False
         where
           alive = extract z
           count = foldl (\s b -> if b then (s+1) else s) 0 doa
           doa = g <$> [shiftL, shiftR, id] <*> [shiftL, shiftR, id]
           g s s' = case s' z of
                        LZip _ z' _ -> extract (s z')

We can then step the state as many times as we want by iteratively extending it with the step function.

We can pretty print the board in a certain neighborhood

pretty :: Int -> Conway -> [String]
pretty n c@(LZip l v r) = (map . map) (\c -> if c then 'o' else ' ') b
    where
      b :: [[Bool]]
      b =  map (around n) (take n l)
                         ++ (tail $ map (around n) (take n r))



around n z = left ++ right
    where
      left = reverse $ map extract (repeatI n shiftL z)
      right = map extract $ tail $ repeatI n shiftR z

and voila, thats the conway game of life modelled beautifully using comonads.
