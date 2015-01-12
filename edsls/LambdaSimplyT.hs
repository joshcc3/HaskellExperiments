{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TypeFamilies #-}
module LambdaSimplyT where

import Control.Applicative
import Control.Arrow
import LambdaCalc
import qualified Data.Map as M
import Control.Monad.State
import Data.Functor.Foldable
import qualified Data.Set as S
import Data.Semigroup 

data CType a = TVar a | CType a :-> CType a deriving (Eq, Ord, Functor)

newtype ZipCType a = ZipCT { unzp :: CType a } deriving (Eq, Ord, Show, 
                                                        Functor, Monad)


instance Applicative CType where
    pure a = pure a :-> pure a
    TVar f <*> TVar a = TVar (f a)
    c <*> (a :-> b) = (c <*> a) :-> (c <*> b)
    (a :-> b) <*> c = (a <*> c) :-> (b <*> c)


instance Monad CType where
    return = pure

    (TVar a) >>= f = f a
    (a :-> b) >>= f = (a >>= f) :-> (b >>= f)

instance Applicative ZipCType where
    pure = ZipCT . pure
    ZipCT (TVar f) <*> ZipCT (TVar a) = ZipCT (TVar (f a))
    (ZipCT (a :-> b)) <*> (ZipCT (c :-> d)) = ZipCT $ (a <*> c) :-> (b <*> d)
    (ZipCT c) <*> (ZipCT c') = ZipCT (c <*> c')


instance Show a =>  Show (CType a) where
    show (TVar s) = show s
    show (TVar a :-> b) = concat [show a, " -> ", show b]
    show (a :-> b) = concat ["(", show a, ")", " -> ", show b]

data CTypeF a n = TVarF a | ArrF n n deriving (Eq, Ord, Show, Functor)


type Context a = M.Map a (CType a)

type Subst a = a -> CType a

type instance Base (CType a) = CTypeF a

instance Foldable (CType a) where
    project (TVar s) = TVarF s
    project (c :-> c') = ArrF c c'

instance Unfoldable (CType a) where
    embed (TVarF s) = TVar s
    embed (ArrF n n') = n :-> n'

tvars :: Ord a => CType a -> S.Set a
tvars = cata tvars'
    where 
      tvars' (TVarF v) = S.singleton v
      tvars' (ArrF l l') = l `S.union` l'


-- Subst is a functor from the Kleisel category to the category of functions
subst :: Subst a -> CType a -> CType a
subst = (=<<)

-- substC is a functor
substC :: Subst a -> Context a -> Context a
substC = fmap M.map subst

-- substP behaves like pure Applicative functor
substP :: (a -> b -> c) -> a -> b -> (c, c)
substP = (.) $ join (&&&)

unify :: CType a -> CType a -> Subst a
unify t t' = undefined -- unzp $ f <$> ZipCT t <*> ZipCT t'
    where 
      f = undefined

{-
unify :: CType -> CType -> Subst
unify (TVar a) t | S.member a (tvars t) = error "Impossible type"
                 | otherwise = M.fromList [(a, t)]
unify (a :-> b) (c :-> d) = comp (unify a c) (unify b d)
unify a b = unify b a


--unifyContexts :: Context -> Context -> Subst
--unifyContexts c c' = unifyContexts' c 
unifyContexts :: Context -> Context -> Subst
unifyContexts c c' = M.foldWithKey g (M.empty) c'
    where 
      g :: String -> CType -> Subst -> Subst
      g tv t s = maybe s (\t' -> unify t t' `comp` s) (M.lookup tv c)
-}
{-
comp :: Subst -> Subst -> Subst
comp s s' = M.LMap (g, m)
    where 
      m = foldl f SM.empty (M.keys s ++ M.keys s')
      f :: SM.Map String CType -> String -> SM.Map String CType
      f cS tv = SM.insert tv (subst s . subst s' $ TVar tv) cS
      g v = undefined


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
  let !s1 = unify p1 (p2 :-> TVar phi) 
      !s2 = uncurry unifyContexts . subst s1 $ (pi1, pi2) -- 
  return $ subst ((s2 `comp` s1)) (M.map getFirst $ M.map First pi1 `M.union` M.map First pi2, TVar phi)
  
      
fresh :: String -> String
fresh "z" = "x0"
fresh [v] = [succ v]
fresh (v:n) = v:show (read n + 1)
-}

ppC :: LambdaT -> StateT String IO (Context String, CType String)
ppC = undefined
