module Parse (Parser(..), token, char, runParse, (<:>), (<||>), (|*|)) where

import Prelude hiding ((*))
import Lang
import Pipes
import Data.Semigroup

newtype Parser b = Parser (Reg IO Char b)

instance Functor Parser where
    fmap f (Parser b) = Parser (fmap f b)


runParse :: Semigroup b => Producer Char IO () -> Parser b -> IO b
runParse p (Parser r) = runReg' p r

token :: String -> t -> Parser t
token s b = Parser $ toRegex' $ tag b s

char :: Char -> t -> Parser t
char s b = Parser $ match s b

(|*|) :: Semigroup t => Parser t -> Parser t
(|*|) (Parser x) = Parser $ star x

(<:>) :: Semigroup t => Parser t -> Parser t -> Parser t
(<:>) (Parser x) (Parser y) = Parser $ x `conc` y

(<||>) :: Parser b -> Parser b -> Parser b
(<||>) (Parser x) (Parser y) = Parser $ x `alter` y

--fastForward (Parser regex) inp final = fmap (iso') *** (Parser . return) $ forward (collapse regex) inp final
