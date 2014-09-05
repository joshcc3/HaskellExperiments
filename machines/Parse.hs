module Parse (token, (<:>), (<||>), (|*|), fastForward) where

import Prelude hiding ((*))
import Vec
import RegExAutomata
import Control.Arrow
import Data.Machine

newtype Parser n b = Parser (Vec n (Mealy Char (St (List b))))

token s b = Parser $ toRegEx $ tag' s (C b Nil)

(|*|) (Parser x) = Parser $ (*) x

(<:>) (Parser x) (Parser y) = Parser $ x <.> y

(<||>) :: Parser n b -> Parser n' b -> Parser (Add n n') b
(<||>) (Parser x) (Parser y) = Parser $ x <|> y

fastForward (Parser regex) inp final = fmap (iso') *** (Parser . (flip Cons Empty)) $ forward (collapse regex) inp final