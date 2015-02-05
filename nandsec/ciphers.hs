{-# LANGUAGE ScopedTypeVariables #-}

import Data.List
import Data.Char 
import Control.Monad

letterInd :: Char -> Int
letterInd c = ord c - ord 'a'


shiftC :: Num a => a -> [a] -> [a]
shiftC = fmap . (+)

substC :: [c] -> [Char] -> [c]
substC k = map (\c -> k !! letterInd c)

polyAlphSubstC :: forall c. [[c]] -> String -> [c]
polyAlphSubstC ks m = fmap f (zip [0..] (fmap (\c -> letterInd c) m))
    where 
      f :: (Int, Int) -> c
      f (ind, col) =  ks !! row !! col
          where 
            row = ind `mod` l
      l = length ks

repeatL :: [a] -> [a]
repeatL k = let x = k++x in x


vigC :: forall a. Num a => [a] -> [a] -> [a]
vigC k m = unfoldr f (repeatL k, m)
    where 
      f :: ([a], [a]) -> Maybe (a, ([a], [a]))
      f (_, []) = Nothing
      f (s:ks, c:cs) = Just (c + s, (ks, cs))

-- vigC as a block cipher
vigC' k m = polyAlphSubstC (map cyclShift k) m
    where 
      cyclShift = undefined


type L = [Bool]
type R = L
type Key = Int
type Block = [L]

block :: (Key -> R -> R) -> L -> R -> [(L, R, Int)]
block f l0 r0 = iterate g (l0, r0, 1)
    where 
      g :: (L, R, Int) -> (L, R, Int)
      g (li, ri, i) = (ri, li `xor` f i ri, i+1)

xor :: R -> R -> R
xor = zipWith xor'
    where 
      xor' a b = a&&b || not a && not b


fstream :: String -> String -> [String]
fstream l0 r0 = fstream' 
    where 
      fstream' = l0 : r0 : zipWith g fstream' (tail fstream')
          where 
            g :: String -> String -> String
            g s s' = ""++s ++ " ⊕ f(" ++ s' ++ ")"

main = do
  [s, s', n] <- fmap words getLine
  let f = fstream s s'
  forever $ do
         n <- fmap read getLine :: IO Int
         putStrLn $ f !! n
         putStrLn ""


         
         


{-
l0
r0
l ⊕ f(r0)
r0 ⊕ f(l0 ⊕ f(r0))
l0 ⊕ f(f(l0 ⊕ f(r0)))
r0 ⊕ f(r0 ⊕ f(l0 ⊕ f(r0)))
l0 ⊕ f(r0 ⊕ f(l0 ⊕ f(l0 ⊕ f(r0))))
r0 ⊕ f(l0 ⊕ f(
-}
