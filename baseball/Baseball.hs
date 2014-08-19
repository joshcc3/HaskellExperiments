module Baseball where

import System.Random
import Data.Char
import Data.List

type GuessRes = (Int, Int)

startup = "Welcome to the baseball game, the rules are blah bla blah"

main = do
  print startup
  print $ "press any key to Start"
  getLine
  gen <- getStdGen
  code <- makeRandomCode initialRange initialListOfNums []gen
  print $ "The random number has been generated"
  loop code []

count = 4
initialRange = 10
initialListOfNums = [0..9]
invalidMsg = "Number is invalid, try again"
c1 :: [Int] -> Bool
c1 l = length l == count
c2 l = count == (length $ nub l)
conditions = [c1,c2]
victoryMsg l = "Congratulations, you guessed it in " ++ show l ++ " guesses."


makeRandomCode range listOfNums ans gen
    | range == initialRange - count = return ans 
    | otherwise  = do
  makeRandomCode (range - 1) (listOfNums \\ [newDig]) (newDig : ans) gen'
  where
    newDig = listOfNums !! (rNo `mod` range)
    (rNo, gen') = next gen

loop code history = do
  print $ "Enter your guess as a 4 digit number"
  putStr ">> "
  s <- getLine
  if (valid s)  then printStuff s ((s, res s) : history) (res s) 
                else print invalidMsg
  if (won (res s)) then genResultReport (length history + 1) >> return () else loop code ((s, res s) : history)
     where
       res = calcGuess code . map digitToInt

       

printStuff guess history s =
  putStrLn "" >> (putStrLn $ take 30 $ repeat '=') >> putStrLn "History> " >> g history  >> (putStrLn $ take 30 $ repeat '=')
  where
    g [] = return ()
    g (l : ls) = prettyPrint l >> g ls

genResultReport l = print $ victoryMsg l


valid :: String -> Bool
valid s = all id $ map ($ (map digitToInt s)) conditions -- c1 l && c2 l

won :: GuessRes -> Bool
won (b, s) = s == count


calcGuess :: [Int] -> [Int] -> GuessRes
calcGuess code guess = (noBalls - noStrikes, noStrikes)
    where
      noStrikes = foldl (\c b -> if b then c+1 else c) 0 $ zipWith (==) code guess
      noBalls = foldl (\c e -> if elem e code then c+1 else c) 0 guess
                

prettyPrint (inp, (ball, strike)) = putStrLn $ "Guess: " ++ inp ++ ": (Balls: "++ show ball ++ ", Strikes: " ++ show strike ++ ")"