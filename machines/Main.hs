module Main where

import Parse

data Token = Digit Int | IF | OPEN_P | CLOSE_P | VAR String | SPC deriving (Eq, Ord, Show)

ifR = token "if" IF
openp = char '(' OPEN_P
closep = char ')' CLOSE_P
spc = char ' ' SPC
regex = openp <:> spc <:> closep 

main = do
  print "Enter string"
  s <- getLine
  print "Enter last"
  final <- getLine
  print $ fst $ fastForward regex s (head final)

  

{-

regex = (|*|) dig <:> char ' ' 

main = do
    s <- getLine
    return ()

-}