module Main where

import Prelude hiding (Either(..), either, (*), (^))
import RegExAutomata
import Control.Applicative hiding ((<|>))
import Data.Monoid 
import Control.Arrow

main = do
    s <- getLine
    if length s == 0 then print "Must be at least 1" >> main else print $ fmap (iso') $ run regex (init s) (last s)

test1 n = map (fmap (length . iso') . uncurry (run regex') . (init &&& last)) s
  where
    s = map (flip Prelude.replicate ' ') [1..n]


f ^ 0 = id
f ^ n = f . (f ^ (n - 1))

f m x = h (g m <> g (m <.> x))
    where 
      g = fmap hom
      h = fmap hom'




regex =  base <.> (base <|> base)
-- (f base ^ 10) base
    where 
      base = ifR <.> spc



-- m <> (m <.> (m <> (m <.> m)))
-- m <> (m <.> m <> m <.> m <.> m)
-- m <> m <.> m <> m <.> m <.> m ... 


regex' = 
  fmap hom' $ foldl1 (<>) $ map (fmap hom . foldl1 (<.>) . flip replicate spc) [1..10]


--(*) m =  fmap hom' (fmap hom m <> fmap hom (m <.> (*)m))
