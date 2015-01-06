{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}

module LambdaCalc where

import qualified Data.Set as S
import Data.Functor.Foldable

type Var = String

data LambdaTF n = VarF Var | LamF Var n | AppF n n 
                  deriving (Eq, Ord, Show, Functor)

data LambdaT = Var Var | Lam Var LambdaT | App LambdaT LambdaT 
                  deriving (Eq, Ord)

instance Show LambdaT where
    show (Var v) = v
    show (Lam v l) = concat ["\\",v, "->", show l]
    show (App l l') = concat [show' l, show' l']
        where 
          show' (Var v) = v
          show' e = "(" ++ show e ++ ")"

type instance Base LambdaT = LambdaTF

instance Foldable LambdaT where
    project (Var v) = VarF v
    project (Lam v l) = LamF v l
    project (App l l') = AppF l l'

instance Unfoldable LambdaT where
    embed (VarF v) = Var v
    embed (LamF v l) = Lam v l
    embed (AppF l l') = App l l'

fv :: LambdaT -> S.Set Var
fv = cata fv'
    where 
      fv' :: LambdaTF (S.Set Var) -> S.Set Var
      fv' (VarF v) = S.singleton v
      fv' (LamF v l) = l S.\\ S.singleton v
      fv' (AppF l l') = S.union l l'


betaR :: LambdaT -> LambdaT
betaR = ana betaR'
    where 
      betaR' :: LambdaT -> LambdaTF LambdaT
      betaR' (Var v) = VarF v
      betaR' (Lam v l) = LamF v l
      betaR' (App (Lam v b) l') = project $ subst l' v b
      betaR' (App l l') = AppF l l'


subst :: LambdaT -> Var -> LambdaT -> LambdaT
subst l v = para subst' 
    where 
      subst' :: LambdaTF (LambdaT, LambdaT) -> LambdaT
      subst' (VarF v') | v' == v = l
                       | otherwise = Var v'
      subst' (AppF (_, n) (_, n')) = App n n'
      subst' (LamF v' (o, n)) | v' == v = Lam v'' n'
                              | otherwise = Lam v' n
          where 
            v'' = nextVar (S.findMax $ fv n)
            n' = subst (Var v'') v' n
            nextVar :: String -> String
            nextVar [] = error "Empty var name"
            nextVar "z" = "x0"
            nextVar [c] = [succ c]
            nextVar (v:num) = v:show (read num + 1)


relabel :: Var -> Var -> LambdaT -> LambdaT
relabel v rv (Var v') | v == v' = Var rv
                      | otherwise = Var v'
relabel v rv (Lam v' l) | v == v' = Lam rv (relabel v rv l)
                        | otherwise = Lam v' (relabel v rv l)
relabel v rv (App l l') = App (relabel v rv l) (relabel v rv l') 

infixl 9 #

(#) :: (a -> b) -> a -> b
(#) f a = f a




