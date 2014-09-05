module Main where

import Prelude hiding (Either(..), either, (*), (^))
import RegExAutomata
import Control.Applicative hiding ((<|>))
import Data.Monoid 
import Control.Arrow

main = do
    s <- getLine
    if length s == 0 then print "Must be at least 1" >> main else print $ fmap (iso') $ fst $ forward ( regex) (init s) (last s)

f ^ 0 = id
f ^ n = f . (f ^ (n - 1))

regex =  (base <|> (base <.> base)) <.> (base <|> (base<.> base))
-- (f base ^ 10) base
    where 
      base = ifR <.> spc



-- m <> (m <.> (m <> (m <.> m)))
-- m <> (m <.> m <> m <.> m <.> m)
-- m <> m <.> m <> m <.> m <.> m ... 



--(*) m =  fmap hom' (fmap hom m <> fmap hom (m <.> (*)m))
