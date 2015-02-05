> {-# LANGUAGE DeriveFunctor #-}

> module PascalsTri where

In the last post we developed an intuition about comonads which we will now put to use.
So comonads allow us calculate values that are the focus within some context.
The extend function allows us to uniformly apply this computation across all values in the comonad as though they had the focus. 

One example of a context dependent calculation is Pascals triangle. A value at the next level is the sum of itself and an adjacent value. 

List Zipper: 

Before we go any further we're going to develop another interesting comonadic structure, a zipper over a stream. The zipper has as its focus some element and extends infintely in both directions. Thus our zipper consists of a stream that extends infinitely in the left direction, the current focus and a list that extends infinitely in the right direction.

> import Control.Comonad

> data LZip a = LZip [a] a [a] deriving (Eq, Ord, Show, Functor)

We want to be able to move around in the structure; we develop two utility functions,

> shiftL :: LZip a -> LZip a
> shiftL (LZip l a r) = LZip (tail l) (head l) (a:r)

This moves the focus to the left pushing the current value to the left.

> shiftR :: LZip a -> LZip a
> shiftR (LZip l a r) = LZip (a:l) (head r) (tail r)

This moves the focus to the right pushing the current value to the left.

So this zipper has an element it focuses on, and has a context (its position in the stream).
That sounds pretty much like the requirement for a comonad and we can indeed derive a comonadic instance for it.

> instance Comonad LZip where
>    extract (LZip _ a _) = a

Extract gives us the current focus in the zipper.

Duplicate has to give us a LZip (LZip a). That structure gives us a stream of parallel streams.
After duplicate the structure that we obtain is such that, the stream that is focused on as we move to the right is the original stream shifted to the right and vice versa for the left.

>    duplicate z@(LZip l v r)
>        = LZip left z right
>          where
>            left = tail $ iterate shiftL z
>            right = tail $ iterate shiftR z

If we represent one layer of Pascals triangle as an infinite stream focused on some relevant location, the step function is:

> addLeft :: LZip Int -> Int
> addLeft z = extract z + extract (shiftL z)

Now the triangle is a 2D object which we can represent by parallel layers. Duplicate allows us to create these parallel layers. Now the step function to create the next layer in the triangle is simply:

> nextLayer :: LZip (LZip Int) -> LZip Int
> nextLayer = extend addLeft . extract . shiftL

And that's all there is to it. The next we would want to do is crop an area of interest. For a one dimensional zipper we have:

> crop :: Int -> LZip a -> [a]
> crop n z = (reverse $ map extract left) ++ (focus:map extract right)
>     where 
>       left = take n (tail $ iterate shiftL z)
>       right = take n (tail $ iterate shiftR z)
>       focus = extract z

Extending this to the two dimensional case is simple, we crop along the second dimesion, and along the first:

> crop2D :: Int -> LZip (LZip Bool) -> [[Bool]]
> crop2D n = map (crop n) . crop n

Finally, the triangle itself consists of the initial value to kick start the process while the successive layers are the result of extending using 'nextLayer'.

> pTri :: LZip (LZip Int)
> pTri = LZip (repeat all0s) gen right
>     where 
>       (_:right) = map nextLayer $ iterate shiftR pTri
>       all0s = LZip (repeat 0) 0 (1:repeat 0)
>       gen = LZip (repeat 0) 1 (repeat 0)



And that's it, Pascals triangle using comonads. The takeaway is that comonads allow us to use a context around a value to determine a result as opposed to monads which focus on creating context from some values. 

Next up - Conways game of life.
