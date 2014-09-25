module ToyLexer where

import Prelude hiding (Either(..), either, (*), (^))
import Parse
import Data.Semigroup
import Control.Applicative

type ToyLexer' = Parse.Parser Char Tok
type ToyLexer = Parse.Parser Char (Last [Tok])

data Tok =  PLUS | MINUS | MULT | DIVISION | INT | EQUALS | O_PARENS | C_PARENS | IF | NEW | SEMI_COLON | Id String | Lit Lit | WS deriving (Ord, Show, Read)

data Lit = INT_LITER String | TRUE | FALSE deriving (Ord, Show, Read)

-- extract types that are equal based on their tags into newtype
instance Eq Tok where
    PLUS == PLUS = True
    MINUS == MINUS = True
    MULT == MULT = True
    DIVISION == DIVISION = True
    INT == INT = True
    EQUALS == EQUALS = True
    O_PARENS == O_PARENS = True
    C_PARENS == C_PARENS = True
    IF == IF = True
    NEW == NEW = True
    SEMI_COLON == SEMI_COLON = True
    Id _  == Id _ = True
    Lit l == Lit l' = l == l'
    WS == WS = True
    _ == _ = False

instance Eq Lit where
    (INT_LITER _) == (INT_LITER _) = True
    TRUE == TRUE = True
    FALSE == FALSE = False
    _ == _ = False

toks :: [(String, Tok)]
toks = [("int", INT), ("=", EQUALS), ("(", O_PARENS), (")", C_PARENS), ("new", NEW), ("if", IF), (";", SEMI_COLON), ("+", PLUS), ("-", MINUS), ("*", MULT), ("/", DIVISION)]

instance Semigroup Lit where
    (<>) (INT_LITER s) (INT_LITER s') = INT_LITER $ s <> s'
    _ <> _ = TRUE

--------------------------------------------------------------------------------

litR = (fmap INT_LITER $ (|*|) digR) <||> token "true" TRUE <||> token "false" FALSE

identR :: ToyLexer'
identR = fmap Id $ (lAlphaR <:> ((|*|) (lAlphaR <||> uAlphaR <||> digR))) <* (char ' ' WS)

tokR :: ToyLexer'
tokR =  identR <||> foldl1 (<||>) (map (uncurry token) toks)  <||> fmap Lit litR
 
ws :: ToyLexer'
ws = fmap (const WS) $ (|*|) $ char ' ' () <||> char '\n' () <||> char '\t' ()

toyR :: ToyLexer
toyR = Last <$> ((|*|) $ ((:[]) <$> tokR))

--------------------------------------------------------------------------------
