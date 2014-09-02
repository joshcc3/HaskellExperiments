module Parse (alphaNum, token, char, dig, (|.|), (<:>), (<||>), (|*|), fastForward) where

import Data.Char
import Prelude hiding ((*))
import RegExAutomata
import Control.Arrow
import Control.Monad (join)
import Data.Machine

newtype Parser b = Parser [Mealy Char (St (List b))] 
token :: String -> b -> Parser b
token s b = Parser $ toRegEx $ tag' s (C b Nil)

char :: Char -> b -> Parser b
char a b = Parser [match a (C b Nil)]

(|.|) = dot

dig = foldl1 (<||>) $ map (uncurry char) $ map (join (,) . head . show) [0..9]

(|*|) :: Parser b -> Parser b
(|*|) (Parser x) = Parser $ (*) x


(<:>) :: Parser b -> Parser b -> Parser b
(<:>) (Parser x) (Parser y) = Parser $ x <.> y

(<||>) :: Parser b -> Parser b -> Parser b
(<||>) (Parser x) (Parser y) = Parser $ x <|> y

fastForward (Parser regex) inp final = fmap (iso') *** (Parser . (:[])) $ forward (collapse regex) inp final

alphaNum = dig <||> (foldl1 (<||>) $ map (uncurry char . join (,) ) ['a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z'])