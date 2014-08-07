{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}

import Control.Applicative
import Data.Fix
import Data.Char
import Control.Monad.Reader
import qualified Data.Map as M

data CFlow b a = If (Exp' b) a a
             | While Bool a
             | Ass String (Exp' b)
             | Seq a a
             | Skip
type St a    = M.Map String a
data Exp a   = Var String | Lit a
type Exp' a  = Reader (St a) (Exp a)


instance Functor Exp' where
    fmap f e = do
      r <- ask
      v <- e
      return $ Lit $ f $ getVal v r
      
instance Applicative Exp' where
    pure x = return $ Lit x
    e <*> e' = do
      r <- ask
      v <- e
      v' <- e'
      return $ Lit $ v v'


instance Num (Exp' Int) where
    e1 + e2 = do
      r <- ask
      v  <- e1
      v' <-  e2
      return $ Lit $ getVal v r + getVal v' r
    
    e1 * e2 = do
      r <- ask
      v  <- e1
      v' <-  e2
      return $ Lit $ getVal v r * getVal v' r
 
    abs e1 = do
      r <- ask
      v <- e1
      return $ Lit $ getVal v r

    fromInteger i = return $ Lit $ fromInteger i

    signum e = do
      r <- ask
      v <- e
      return $ Lit $ signum $ getVal v r


getVal (Lit n) r     = n
getVal (Var ident) r = al $ M.lookup ident r 
    where 
      al (Just x) = x

-- | Create an F-Algebra.

instance Functor (CFlow b) where
    fmap f (If b t e)  = If b (f t) (f e)
    fmap f (While b c) = While b $ f c
    fmap f (Seq c c')  = Seq (f c) (f c')
    fmap f Skip        = Skip

--alg :: CFlow a -> a
--alg (If b t e) 