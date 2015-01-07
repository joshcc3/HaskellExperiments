{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TypeFamilies #-}
module LambdaSimplyT where

import LambdaCalc
import qualified Map as M
import Control.Monad.State
import Data.Functor.Foldable
import qualified Data.Set as S
import Data.Semigroup 
import qualified Data.Map as SM

data CType = TVar String | CType :-> CType deriving (Eq, Ord)

instance Show CType where
    show (TVar s) = s
    show (TVar a :-> b) = concat [a, " -> ", show b]
    show (a :-> b) = concat ["(", show a, ")", " -> ", show b]

data CTypeF n = TVarF String | ArrF n n deriving (Eq, Ord, Show, Functor)

type Context = M.LazyMap String CType

type St = String

type Subst = M.LazyMap String CType

type instance Base CType = CTypeF

instance Foldable CType where
    project (TVar s) = TVarF s
    project (c :-> c') = ArrF c c'

instance Unfoldable CType where
    embed (TVarF s) = TVar s
    embed (ArrF n n') = n :-> n'

tvars :: CType -> S.Set String
tvars = cata tvars'
    where 
      tvars' :: CTypeF (S.Set String) -> S.Set String
      tvars' (TVarF v) = S.singleton v
      tvars' (ArrF l l') = l `S.union` l'


class Substit a where
    subst :: Subst -> a -> a
                
instance Substit CType where
    subst s (TVar v) = maybe (TVar v) id (M.lookup v s)
    subst s (a :-> b) = subst s a :-> subst s b

instance Substit Context where
    subst s c = M.map (subst s) c

instance (Substit a, Substit b) => Substit (a, b) where
    subst s (a, b) = (subst s a, subst s b)

unify :: CType -> CType -> Subst
unify (TVar a) t | S.member a (tvars t) = error "Impossible type"
                 | otherwise = M.fromList [(a, t)]
unify (a :-> b) (c :-> d) = comp (unify a c) (unify b d)
unify a b = unify b a


unifyContexts :: Context -> Context -> Subst
unifyContexts c c' = M.foldWithKey f (M.toLMap (Just . TVar) SM.empty) c'
    where 
      f :: String -> CType -> Subst -> Subst
      f tv t s = maybe s (\t' -> comp (unify t t') s) (M.lookup tv c)


comp :: Subst -> Subst -> Subst
comp s s' = M.map f s' 
    where 
      f :: CType -> CType
      f = subst s


ppC :: LambdaT -> StateT St IO (Context, CType)
ppC (Var x) = do
  s <- get
  let s' = fresh s
  put s'
  return (M.fromList [(x, TVar s')], TVar s')
ppC (Lam x m) = do
    (!pi, t) <- ppC m
    let xT = M.lookup x pi
    maybe (def pi t) (\a -> return (M.insert x (a :-> t) pi, a :-> t)) xT
    where 
      def pi t = do 
        s <- get
        put (fresh s)
        let resT = (TVar (fresh s) :-> t)
        return (M.insert x resT pi, resT)
ppC (App m n) = do
  s <- get
  let phi = fresh s
  put phi
  (pi1, p1) <- ppC  m  -- p1 = c
  (pi2, p2) <- ppC n -- p2 = d
  let !s1 = unify p1 (p2 :-> TVar phi) -- unify c (d -> b) = c -> (d -> b)
      !s2 = uncurry unifyContexts . subst s1 $ (pi1, pi2) -- 
  return $ subst ((s2 `comp` s1)) (M.map getFirst $ M.map First pi1 `M.union` M.map First pi2, TVar phi)
  
      

fresh :: String -> String
fresh "z" = "x0"
fresh [v] = [succ v]
fresh (v:n) = v:show (read n + 1)
