{-# LANGUAGE ViewPatterns, PatternSynonyms, StandaloneDeriving,
      DeriveFunctor, DeriveFoldable, UndecidableInstances #-}

module Interpreter where

import Data.Foldable

data ExprF r
  = AppF r r
  | AbsF String r
  | VarF String
  | LitIF Int
  | AddF r r
  | MulF r r
  deriving (Show, Functor, Foldable)

newtype Fix f = Fix {unfix :: f (Fix f)}

deriving instance (Show (f (Fix f))) => Show (Fix f)


type Expr = Fix ExprF

pattern App a b = Fix (AppF a b)
pattern Abs i b = Fix (AbsF i b)
pattern Var i = Fix (VarF i)
pattern LitI v = Fix (LitIF v)
pattern Add a b = Fix (AddF a b)
pattern Mul a b = Fix (MulF a b)

add1 :: Expr
-- add1 = Fix $ AbsF "n" (Fix $ AddF (Fix $ VarF "n") (Fix $ LitIF 1))
add1 = Abs "n" (Add (Var "n") (LitI 1))

add1to4 :: Expr
add1to4 = App add1 (LitI 4)

type Env = [(String, Expr)]

eval :: Env -> Expr -> Expr
--eval e (App a' b') = let Abs i a = eval a' in let b = eval b' in ...
eval e (App (eval e -> Abs i a) (eval e -> b)) = eval ((i, b) : e) a
eval e (Var (flip lookup e -> Just v)) = v
eval e (Add (eval e -> LitI a) (eval e -> LitI b)) = LitI (a + b)
eval e (Mul (eval e -> LitI a) (eval e -> LitI b)) = LitI (a * b)
eval _ a = a

cata :: (Functor f) => (f a -> a) -> Fix f -> a
cata alg = alg . fmap (cata alg) . unfix

data Value
  = VLitI Int
  | VFunc (Value -> Value)

instance Show Value where
  show (VLitI v) = show v
  show (VFunc _) = "Function"

type VEnv = [(String, Value)]

evalAlg :: ExprF (VEnv -> Value) -> VEnv -> Value
evalAlg (AppF fa fb) e | (VFunc a) <- fa e = a (fb e)
evalAlg (AbsF i fb) e = VFunc (\v -> fb ((i, v) : e))
evalAlg (VarF i) e | Just v <- lookup i e = v
evalAlg (LitIF v) _ = VLitI v
evalAlg (AddF fa fb) e | VLitI a <- fa e, VLitI b <- fb e = VLitI (a + b)
evalAlg (MulF fa fb) e | VLitI a <- fa e, VLitI b <- fb e = VLitI (a * b)

eval' :: Expr -> Value
eval' x = cata evalAlg x []

litAlg :: ExprF [Int] -> [Int]
litAlg (LitIF v) = [v]
litAlg x = fold x
