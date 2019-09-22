{-# language DeriveFunctor, DeriveTraversable, DeriveFoldable, DeriveGeneric, DeriveAnyClass #-}

module TypeInference where

import Prelude hiding (True, False, and, negate)
import qualified Prelude as P
import Control.Monad
import Control.Unification
import Control.Unification.IntVar
import Control.Unification.Types
import Data.Functor.Fixedpoint
import Data.Traversable
import GHC.Generics

infixl 3 <|>

(<|>) :: Either a b -> Either a b -> Either a b
a@(Right _) <|> _  = a
_ <|> b = b

assert :: Bool -> String -> Either Failure ()
assert P.True _ = Right ()
assert P.False m = Left m  

infixr 5 :->

data TypeF a = TBool | (:->) a a
  deriving (Eq, Show, Functor, Foldable,  Traversable, Generic1, Unifiable)

type Type = Fix TypeF

type PolyType = UTerm TypeF IntVar
type Failure = UFailure TypeF IntVar

type Ident = Char

data Term = True | False | If Term Term Term | Var Ident | Abs Ident Term | App Term Term
  deriving (Show)

type Env = Ident -> Either Failure PolyType

emptyEnv :: Env
emptyEnv _ = Left "Unknown variable"

inferType :: Env -> Term -> IntBindingT TypeF Identity (Either Failure PolyType)
-- inferType _ True = Right TBool
-- inferType _ False = Right TBool
-- inferType e (If c t f) = do
--   tc <- inferType e c
--   assert (tc == TBool) "Bool expected in condition of If"
--   tt <- inferType e t
--   tf <- inferType e f
--   assert (tt == tf) "Branches must be the same type in If"
--   pure tt
-- inferType e (Var i) = e i
-- -- inferType e (Abs i a) = do

-- inferType e (App f a) = do
--   tf <- inferType e f
--   ta <- inferType e a
--   case tf of
--     (tb :-> tc) -> do
--       assert (ta == tb) "Function argument doesn't match"
--       pure tc
--     _ -> Left "Not a function"


negate :: Term
negate = Abs 'x' $ If (Var 'x') False True

and :: Term
and = Abs 'a' $ Abs 'b' $ If (Var 'a') (Var 'b') False

nand :: Term
nand = Abs 'a' $ Abs 'b' $ App negate $ App (App and (Var 'a')) (Var 'b') 