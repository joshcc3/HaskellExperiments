{-# LANGUAGE RankNTypes, ExistentialQuantification #-}

module Main where

import Parse

data Token = Digit Int | IF | OPEN_P | CLOSE_P | VAR String

ifR = token "if" IF
openp = char '(' OPEN_P
closep = char ')' CLOSE_P
ident = (|*|) alphaNum

main = do
  print "Enter string"
  s <- getLine
  print "Enter last"
  final <- getLine
  print $ fst $ fastForward ident s (head final)

  

{-

regex = (|*|) dig <:> char ' ' 

main = do
    s <- getLine
    return ()

-}