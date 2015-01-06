module LambdaTerms where

import LambdaCalc

var :: Var -> LambdaT
var v = Var v

lam :: String -> LambdaT -> LambdaT
lam v l = Lam v l

app :: LambdaT -> LambdaT -> LambdaT
app l l' = App l l'

lam' :: [String] -> LambdaT -> LambdaT
lam' v l = foldr lam l v

iC :: LambdaT
iC = lam "x" (var "x")

sC :: LambdaT
sC = lam' ["x", "y", "z"] $
      app (app (var "x") (var "z")) (app (var "y") (var "z"))


kC :: LambdaT 
kC = lam' ["x", "y"] $
      var "x"

yC :: LambdaT
yC = lam "f" $
      app yC' yC'
    where 
      yC' = lam "x" $
             app (var "f") (app (var "x") (var "x"))




selfApp :: LambdaT
selfApp = lam "x" $ app (var "x") (var "x")

loop :: LambdaT
loop = app selfApp selfApp


