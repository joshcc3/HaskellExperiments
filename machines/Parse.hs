module Parse where

import Prelude hiding ((*))
import Lang
import Pipes
import Data.Semigroup
import Control.Applicative
import Data.Char

newtype Parser a b = Parser (Reg IO a b)

instance Functor (Parser a) where
    fmap f (Parser b) = Parser (fmap f b)

instance Applicative (Parser a) where
    pure x = Parser (pure x)
    (Parser b) <*> (Parser b') = Parser (b <*> b')


runParse :: (Eq a, Semigroup b) => Producer a IO () -> Parser a b -> IO b
runParse p (Parser r) = runReg' p r

token :: String -> t -> Parser Char t
token s b = Parser $ toRegex' $ tag b s

char :: Char -> t -> Parser Char t
char s b = Parser $ Lang.match s b

match :: Eq a => a -> b -> Parser a b
match c b = Parser $ Lang.match c b 

match' :: Eq a => a -> Parser a a
match' = Parser . Lang.match'

(|*|) :: Semigroup t => Parser a t -> Parser a t
(|*|) (Parser x) = Parser $ star x

(<:>) :: Semigroup t => Parser a t -> Parser a t -> Parser a t
(<:>) (Parser x) (Parser y) = Parser $ x `conc` y

(<||>) :: Parser a b -> Parser a b -> Parser a b
(<||>) (Parser x) (Parser y) = Parser $ x `alter` y

--fastForward (Parser regex) inp final = fmap (iso') *** (Parser . return) $ forward (collapse regex) inp final


uAlpha :: [Char]
uAlpha = map chr [65..90]

lAlpha :: [Char]
lAlpha = map chr [97..122]
--
nums :: [Char]
nums = concatMap show  ([0..9] :: [Int])
--
lAlphaR = foldl1 (<||>) [char c [c] | c <- lAlpha]

uAlphaR = foldl1 (<||>) [char c [c] | c <- uAlpha]

digR = foldl1 (<||>) [char c [c] | c <- nums]


prod :: Monad m => String -> Producer Char m ()
prod s = foldl1 (>>) $ Prelude.map yield s
