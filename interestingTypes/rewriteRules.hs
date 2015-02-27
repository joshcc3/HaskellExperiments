{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE Arrows #-}

module Rewrite where

import Data.Monoid
import qualified Data.Set as S
import Control.Arrow
import Control.Applicative

data CTL a = T | F | Var a | Not (CTL a) | CTL a :|| CTL a | CTL a :&& CTL a | CTL a :-> CTL a
           | EX (CTL a) | EG (CTL a) | EU (CTL a, CTL a) | AF (CTL a)
           | AX (CTL a) | AG (CTL a) | AU (CTL a, CTL a) | EF (CTL a) deriving (Eq, Ord, Show)

type State = S.Set String


data Step a = Return State | State :-- Step a | Union (Step a) (Step a)
            | Inter (Step a) (Step a) | Rewrite (Step a)
            | SATex (CTL a) | SATaf (CTL a) | SATeu (CTL a, CTL a)
            | CTL (CTL a) deriving (Eq, Ord, Show)

step :: State -> (a -> State) -> Step a -> Either State (Step a)
step _ _ (Return s) = Left s
step s m (st :-- stp) = either (Left . S.difference st) (Right . (st :--)) (step s m stp)
step s m (Union ctl ctl') = res n n'
    where
      n = step s m ctl
      n' = step s m ctl'
      res (Left x) (Left y) = Left (x <> y)
      res (Left x) (Right y) = Right (Union (Return x) y)
      res (Right x) (Left y) = Right (Union x (Return y))
      res x y = Union <$> x <*> y
step s m (Inter ctl ctl') = res n n'
    where
      n = step s m ctl
      n' = step s m ctl'
      res (Left x) (Left y) = Left (S.intersection x y)
      res (Left x) (Right y) = Right (Union (Return x) y)
      res (Right x) (Left y) = Right (Union x (Return y))
      res x y = Union <$> x <*> y
step s m (Rewrite st) = step s m st
step s m (SATex ctl) = Left $ satex s m ctl
step s m (SATaf ctl) = Left $ sataf s m ctl
step s m (SATeu (ctl, ctl')) = Left $ sateu s m ctl ctl'
step s m (CTL ctl) = Right $ eval s m ctl

satex :: State -> (a -> State) -> CTL a -> State
satex s m ctl =

sataf :: State -> (a -> State) -> CTL a -> State
sataf s m ctl = S.empty

sateu :: State -> (a -> State) -> CTL a -> CTL a -> State
sateu s m ctl ctl' = S.empty


eval :: State -> (a -> State) -> CTL a -> Step a
eval s _ T  = Return s
eval _ _ F = Return S.empty
eval s m (Var a) = Return $ m a
eval s m (Not a) = s :-- (CTL a)
eval s m (a :|| b) = Union (CTL a) (CTL b)
eval s m (a :&& b) = Inter (CTL a) (CTL b)
eval s m (a :-> b) = Rewrite . CTL $ (Not a :|| b)
eval s m (AX p) = Rewrite . CTL $ (Not (EX (Not p)))
eval s m (AU (p1, p2)) = Rewrite . CTL $ (Not(EU(Not p2, Not p1 :&& Not p2) :|| EG (Not p2)))
eval s m (EF p) = Rewrite . CTL $ (EU (T, p))
eval s m (EG p) = Rewrite . CTL $ (Not (AF (Not p)))
eval s m (AG p1) = Rewrite . CTL $ (Not (EF (Not p1)))
eval s m (EX p1) = SATex p1
eval s m (AF p1) = SATaf p1
eval s m (EU (p1, p2)) = SATeu (p1, p2)
