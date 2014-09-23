module Main where

import Prelude hiding (Either(..), either, (*), (^))
import Data.Char
import Parse
import Data.Semigroup
import Pipes

main :: IO ()
main = do
  s <- getLine
  b <- runParse (prod s) toyR
  (print . filter (/= WS) . getLast) b
{-
int <var name> = <int literal>
int [] <var name> = new int [<int literal>]
if ( var name | bool literal ) {
  stat
}
-}

data Tok =  INT | EQUALS | O_PARENS | C_PARENS | IF | NEW | SEMI_COLON
          | Id String | Lit Lit | WS deriving (Eq, Ord, Show, Read)

data Lit = INT_LITER String | TRUE | FALSE deriving (Eq, Ord, Show, Read)

toks :: [(String, Tok)]
toks = [("int", INT), ("=", EQUALS), ("(", O_PARENS), (")", C_PARENS), ("new", NEW), ("if", IF), (";", SEMI_COLON)]

instance Semigroup Lit where
    (<>) (INT_LITER s) (INT_LITER s') = INT_LITER $ s <> s'
    _ <> _ = TRUE

--------------------------------------------------------------------------------

uAlpha :: [Char]
uAlpha = map chr [65..90]

lAlpha :: [Char]
lAlpha = map chr [97..122]

nums :: [Char]
nums = concatMap show  ([0..9] :: [Int])

lAlphaR :: Parse.Parser String
lAlphaR = foldl1 (<||>) [char c [c] | c <- lAlpha]

uAlphaR :: Parse.Parser String
uAlphaR = foldl1 (<||>) [char c [c] | c <- uAlpha]

digR :: Parse.Parser String
digR = foldl1 (<||>) [char c [c] | c <- nums]

litR :: Parse.Parser Lit
litR = (fmap INT_LITER $ (|*|) digR) <||> token "true" TRUE <||> token "false" FALSE

identR :: Parse.Parser Tok
identR = fmap Id $ lAlphaR <:> ((|*|) (lAlphaR <||> uAlphaR <||> digR))

tokR :: Parse.Parser Tok
tokR = foldl1 (<||>) (map (uncurry token) toks) <||> identR <||> fmap Lit litR
 
ws :: Parse.Parser Tok
ws = fmap (const WS) $ (|*|) $ char ' ' () <||> char '\n' () <||> char '\t' ()

toyR :: Parse.Parser (Last [Tok])
toyR = fmap Last $ (|*|) $ fmap (:[]) tokR <:> fmap (:[]) ws

--------------------------------------------------------------------------------

prod :: Monad m => String -> Producer Char m ()
prod s = foldl1 (>>) $ Prelude.map yield s
