module ToyParser where

import ToyLexer
import Parse
import Control.Applicative
import Control.Monad.Identity
import Data.Semigroup

{-
prog = stat
stat = stat ; stat | int <var name> = <int literal> | 
       int [] <var name> = new int [exp] | 
       <var name> = exp | 
       if ( exp ) (
          stat
        ) | Skip

lit = <int literal> | <bool literal> | <ident>
exp = lit | exp - exp | exp + exp | exp * exp | exp / exp | (exp)

-}

data Ast = STAT Stat

data Stat = SEQ Stat Stat | DECL String Int | AssI String (Exp Int) | AssB String (Exp Bool) | IF_STAT (Exp Bool) Stat | Skip

type Literal a = Identity a
    
type Identifier a = Identity String

-- Add boolean operators
data Exp a = ID (Identifier a) | LIT (Literal a) | SUB (Exp a) (Exp a) | ADD (Exp a) (Exp a) | MUL (Exp a) (Exp a) | DIV (Exp a) (Exp a) | BRACKET (Exp a)

type ToyParser = Parse.Parser Tok Ast

class Literal' a where
    litP :: Parser Tok (Literal a)

instance Literal' Int where
    litP = (\(Lit (INT_LITER x)) -> return $ read x) <$> match' (Lit (INT_LITER undefined)) 

instance Literal' Bool where
    litP = match (Lit TRUE) (return True) <||> match (Lit FALSE) (return False)

class Exp' a where
    expP :: Parser Tok (Exp a)
    
instance Exp' Int where
    expP =  (LIT <$> litP) <||>
            (SUB <$> expP <*> expP) <||>
            (ADD <$> expP <*> expP) <||>
            (MUL <$> expP <*> expP) <||>
            (DIV <$> expP <*> expP) <||>
            ((ID . return) <$> ((\(Id s) -> s) <$> match' (Id undefined)))

instance Exp' Bool where
    expP = (LIT <$> litP) <||> 
           ((ID . return) <$> ((\(Id s) -> s) <$> match' (Id undefined)))   

statP :: Parser Tok Stat
statP = (SEQ <$> statP <*> statP) <||>
        declI <||>
        assIP

declI :: Parser Tok Stat
declI = (AssI . getLast) <$> (match INT Nothing *> 
                              match WS Nothing *> 
                              ((\(Id x) -> Last x) <$> match' (Id undefined))) 
        <*> expP

-- Add a semigroup which is annotated with information of empty or not
assIP :: Parser Tok Stat
assIP = (AssI . getFirst) <$> (((\(Id x) -> First x) <$> match' (Id undefined)) <* match EQUALS Nothing) <*> expP

assBP :: Parser Tok Stat
assBP = (AssB . getFirst) <$> (((\(Id x) -> First x) <$> match' (Id undefined)) <* match EQUALS Nothing) <*> expP
            


{-
prog = stat
stat = stat ; stat | int <var name> = <int literal> | 
       int [] <var name> = new int [exp] | 
       <var name> = exp | 
       if ( exp ) (
          stat
        ) | Skip

lit = <int literal> | <bool literal> | <ident>
exp = lit | exp - exp | exp + exp | exp * exp | exp / exp | (exp)

-}

    
