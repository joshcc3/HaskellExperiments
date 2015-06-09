import Data.Machine
import System.Random


genN' 0 _ = []
genN' i g = f (next g)
    where 
      f (n, g') = n:genN' (i-1) g'

genN i s = genN' i (mkStdGen s)

findN1 n = last . genN n
      
machine g = unfoldMoore h g
    where 
      h g = (i, f)
          where 
            (i, g') = next g
            f False = g'
            f True = snd . next $ g

driveMoore :: Moore a b -> [a] -> [b]
driveMoore m [] = []
driveMoore (Moore b f) (a:as) = b:driveMoore (f a) as

findN2 n s = last (driveMoore (machine randGen) bin)
    where 
      randGen = mkStdGen s
      bin = toBin n
      toBin 0 = [False]
      toBin i = uncurry f (divMod i 2)
          where 
            f i j = h j:toBin i
            h 0 = False
            h 1 = True
