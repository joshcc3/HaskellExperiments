{-# LANGUAGE FlexibleInstances, TypeSynonymInstances, RankNTypes, ScopedTypeVariables #-}

module While where


import Control.Monad
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Identity
import qualified Data.Map as M
import Control.Applicative
import Control.Monad.Free

type Res b = StateT (St b) IO ()

type St b = M.Map String b

type Expr a b = ReaderT a Maybe b

type Expr' b = Expr (St b) b 

data CFlow b a =   If (Expr' Bool) a a
               | While (Expr' Bool) a
               | Seq a a
               | Ass String (Expr' b)
               | Skip

instance Functor (CFlow b) where
    fmap f (If b t e) = If b (f t) (f e)
    fmap f (While c s) = While c (f s)
    fmap f (Seq s s') = Seq (f s) (f s')
    fmap f (Ass s e) = Ass s e
    fmap f Skip = Skip

-- Create an F-Algebra with the endofunctor CFlow

alg Skip = return ()
alg (Ass s e) = do
  state <- get
  put $ M.update (const $ runReaderT e state) s state
  return ()
alg (If b t e) =  get >>= f . runReaderT b >> return ()
    where 
      f Nothing = alg Skip
      f (Just b) = if b then t else e
alg (Seq s s') = s >> s'
alg w@(While c s) = alg $ If c (s >> alg w) $ alg Skip



               
instance Num a => Num (Expr' a) where
    e + e' = (+) <$> e <*> e'
    e * e' = (*) <$> e <*> e'
    abs = (abs <$>)
    signum = (signum <$>)
    fromInteger i = return $ fromInteger i

-- | Convenience boolean oeprators
e ~== e' = (==) <$> e <*> e'
e ~&& e' = (&&) <$> e <*> e'
e ~|| e' = (||) <$> e <*> e'
e ~> e' = (>) <$> e <*> e'
e ~< e' = (<) <$> e <*> e'

var :: String -> Expr' a
var s = ask >>= lift . M.lookup s

exp1 :: Expr' Int
exp1 = 3 + 4 + var "s"

main = do
    print $ runReaderT exp1 $ M.fromList []
