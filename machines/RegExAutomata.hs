{-# LANGUAGE RankNTypes, ScopedTypeVariables #-}

module Main where
    
import Prelude hiding ((.), id, (*))
import Control.Applicative
import Machine
import Data.Monoid
import Control.Monad

type RegExAut b = Moore Char (St b)
data St a       = A a | N | Err deriving (Eq, Show)
data Tok = IF | FOR | OPEN_P | CLOSE_P | INT_LIT Int | SEMI_COLON | VAR Char | EQUALS


{- So what we would like to do is compose the machines together. composing machines would sort've be akin
   to connecting them end to end. However the problem comes when you want to connect machines and theyre states 
    conflict. Therefore it must only be possible to connect machines when their states are the same, or are synchronized
    that is they are in lock step. Thus for machines to compose we will need some further restrictions. Let us think about how 
    we would go about -}

{- Another thing that we would like to do is run machines in parallel. That is simulate an nfa. This seems to have the characteristics
    of a monoid. A monoid has two morphisms, identity, and the append operation. Appending a machine means running it in parallel.
-}

{-
So what we want to do, is create a pipe that represents a stream of tokens. We want to create a moore machine for each of the tokens.
Then we want to append these machines together to be able to parse a string. 
-}

-- So the first step is to construct moore machines for each of the tokens.
-- what would be nice is if we could construct the machine for individual characters.
-- then compose machines together to lex entire tokens and finally compose those machines
-- to parse multiple tokens


instance Functor St where
    fmap f (A a) = A $ f a
    fmap f N = N
    fmap f Err = Err

instance Monoid a => Monoid (St a) where
    mempty = Err
    mappend (A a) (A a') = A $ a <> a'
    mappend _ (A a) = A a
    mappend (A a) _ = A a
    mappend Err Err = Err
    mappend _ _ = N

--cm :: (Eq a) => a -> Moore a (St [b])
cm c b = Moore N $ \c' -> if c == c' then pure $A [b] else pure Err

(.) :: (Monoid b) => Moore a (St b) -> Moore a(St b) -> Moore a (St b)
(.) (Moore N f) m = Moore N $ \c -> f c . m 
(.) (Moore (A a) f) m = pure (A a) <> m
(.) (Moore Err f) _ = pure Err

(*) :: Moore a  (St [b]) -> Moore a (St [b])
(*) m = m . (*)m


--toRegEx :: (Eq a, Monoid b) => [(a, b)] -> Moore a (St b)
toRegEx s = foldl1 (.) $ map (uncurry cm) s 

      
tag :: [b] -> [(b, [b])]
tag = map $ (fmap (:[]) . join (,))
      where
        (.) f g x = f (g x)

ifR = toRegEx $ tag "if"
forR = toRegEx $ tag "for"
openP = toRegEx $ tag "("
closeP = toRegEx $ tag ")"
spc = toRegEx $ tag " "

run :: [a] -> Moore a (St b) -> Moore a (St b)
run [] m = pure $ view m
run (c:cs) m = run cs (runMoore m c)

main = do
    s <- getLine
    print $ view $ run s $ (*) $ ifR . spc . forR