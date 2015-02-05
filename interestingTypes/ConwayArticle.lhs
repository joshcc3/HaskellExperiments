> {-# LANGUAGE DeriveFunctor #-}

> module Conway where

> import Prelude hiding ((^))
> import Control.Comonad
> import PascalsTri
> import Control.Applicative

Conways game of life consists of an environment with synchronous development happening across all of the cells. The status of the cell is decided by its current status and its environment.
A cells status is simply that of dead or alive for which we will use booleans.

That's a function of the form:

step :: Env Bool -> Bool

The rules are as follows:


We need to be able to step to the left, right, and up and down.
This is where the list zipper comes into play. A duplicated zipper will represent the environment of
a particular process.

> type Conway = LZip (LZip Bool)

Finally deciding whether or not a cell stays alive till the next iteration is as simply as
observing the status of the surroundings cells and applying the rules.

> shiftU :: LZip (LZip Bool) -> LZip (LZip Bool)
> shiftU = fmap shiftR


> shiftD :: LZip (LZip Bool) -> LZip (LZip Bool)
> shiftD = fmap shiftL

> decide :: Conway -> Bool 
> decide z | count < 2 = False 
>          | count == 3 = True 
>          | alive && count == 2 = True 
>          | otherwise = False 
>          where 
>            alive = extract . extract $ z 
>            count = if alive then count' - 1 else count'
>            count' = foldl (\s b -> if b then (s+1) else s) 0 doa 
>            doa = g <$> [shiftL, shiftR, id] <*> [shiftL, shiftR, id] 
>            g s s' = case s' z of 
>                         LZip _ z' _ -> extract (s z') 

> step :: Conway -> LZip (LZip Bool)
> step z = extend g z

shiftL z

> g v = LZip l c r
>   where 
>     c = decide v
>     l = fmap decide (tail $ iterate shiftD v)
>     r = fmap decide (tail $ iterate shiftU v)



We can then step the state as many times as we want by iteratively extending it with the step function.

We can pretty print the board in a certain neighborhood

> pp1 n = putStrLn . map visual . crop n 
> visual = (\c -> if c then 'o' else ' ')

> pretty :: Int -> Conway -> [String]
> pretty n c@(LZip l v r) = (map . map) visual b
>     where
>       b :: [[Bool]]
>       b = crop2D n c

and voila, thats the conway game of life modelled beautifully using comonads.

> initial :: Conway
> initial = LZip (repeat allFalses) blinker (repeat allFalses)
>     where 
>       allFalses = LZip (repeat False) False (repeat False)
>       blinker = LZip (True:repeat False) True (True:repeat False)


> pp :: Int -> Conway -> IO ()
> pp i c = mapM_ putStrLn (pretty i c)

> _ ^ 0 = id
> f ^ n = f . (f ^ (n-1))
