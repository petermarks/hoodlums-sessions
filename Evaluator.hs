{-# LANGUAGE TypeFamilies, TypeSynonymInstances, StandaloneDeriving, FlexibleContexts,
    UndecidableInstances #-}

module Evaluator where

import Prelude hiding (sum)
import qualified Prelude as P
import Control.Applicative
import Control.Monad.Identity

class ExprSym e where
  litI :: Int -> e Int
  litL :: [e a] -> e [a]
  add :: e Int -> e Int -> e Int
  sum :: e [Int] -> e Int

instance ExprSym Identity where
  litI = pure
  litL = sequence
  add = liftA2 (+)
  sum = fmap P.sum

evaluate :: Identity a -> a
evaluate = runIdentity

data IntExpr
  = LitI Int
  | Add IntExpr IntExpr
  | Sum (ListExpr Int)
  deriving Show

data ListExpr a
  = LitL [ExprType a]

deriving instance (Show (ExprType a)) => Show (ListExpr a)

type family ExprType a

type instance ExprType Int = IntExpr

type instance ExprType [a] = ListExpr a

newtype Expr t = Expr {unExpr :: ExprType t}

instance ExprSym Expr where
  litI = Expr . LitI
  litL = Expr . LitL . map unExpr
  add a b = Expr $ Add (unExpr a) (unExpr b)
  sum = Expr . Sum . unExpr
