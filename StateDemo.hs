module Test where

import Control.Monad
import Control.Monad.State


push :: a -> State [a] ()
push v
 = do
    s <- get
    put (v:s)

pop :: State [a] a
pop 
 = do
   (v:vs) <- get
   put vs
   return v

main
 = do
   get
   push 1
   push 2
   push 3
   x <- pop
   y <- pop
   push x
   push y



