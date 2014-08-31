module Parse (token, (<:>), (<||>), (|*|), fastForward) where

import Prelude hiding ((*))
import RegExAutomata
import Control.Arrow
import Data.Machine

newtype Parser b = Parser [Mealy Char (St (List b))] 

token :: String -> b -> Parser b
token s b = Parser $ toRegEx $ tag' s (C b Nil)

(|*|) :: Parser b -> Parser b
(|*|) (Parser x) = Parser $ (*) x


(<:>) :: Parser b -> Parser b -> Parser b
(<:>) (Parser x) (Parser y) = Parser $ x <.> y

(<||>) :: Parser b -> Parser b -> Parser b
(<||>) (Parser x) (Parser y) = Parser $ x <|> y

fastForward (Parser regex) inp final = fmap (iso') *** (Parser . (:[])) $ forward (collapse regex) inp final