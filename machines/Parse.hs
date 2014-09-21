module Parse (token, (<:>), (<||>), (|*|)) where

import Prelude hiding ((*))
import Data.Monoid
import Lang

newtype Parser b = Parser (Reg Char b)

instance Functor Parser where
    fmap f (Parser b) = Parser (fmap f b)


token :: Monoid t => String -> t -> Parser t
token s b = Parser $ toRegex $ tag b s

(|*|) :: Monoid t => Parser t -> Parser t
(|*|) (Parser x) = Parser $ star x

(<:>) :: Monoid t => Parser t -> Parser t -> Parser t
(<:>) (Parser x) (Parser y) = Parser $ x `conc` y

(<||>) :: Monoid b => Parser b -> Parser b -> Parser b
(<||>) (Parser x) (Parser y) = Parser $ x `alter` y

--fastForward (Parser regex) inp final = fmap (iso') *** (Parser . return) $ forward (collapse regex) inp final
