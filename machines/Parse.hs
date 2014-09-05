module Parse (token, (<:>), (<||>), (|*|)) where

import Prelude hiding ((*))
import RegExAutomata
import Control.Arrow
import Data.Machine

newtype Parser n b = Parser (Mealy Char (St (List b)))

token s b = Parser $ toRegEx $ tag' s (C b Nil)

(|*|) (Parser x) = Parser $ (*) x

(<:>) (Parser x) (Parser y) = Parser $ x <.> y

(<||>) (Parser x) (Parser y) = Parser $ x <|> y

--fastForward (Parser regex) inp final = fmap (iso') *** (Parser) $ forward (collapse regex) inp final