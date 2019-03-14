module ContextualExpressions where

import Prelude hiding (sum)

data Expr
  = Lit Double
  | Var Int
  | Sum [Expr]
  deriving (Show)

data R
  = Value Expr
  | Let Double R
  deriving (Show)

shift :: Expr -> Expr
shift (Lit x) = Lit x
shift (Var n) = Var $ n + 1
shift (Sum es) = Sum $ map shift es

sum :: [R] -> R
sum = sum' [] 

sum' :: [Expr] -> [R] -> R
sum' es [] = Value $ Sum $ reverse es
sum' es (Value e : rs) = sum' (e : es) rs
sum' es (Let x r : rs) = Let x $ sum' (map shift es) (r : rs) 

example1 :: [R]
example1 = [Value $ Lit 3, Value $ Lit 4]

result1 :: R
result1 = Value $ Sum [Lit 3, Lit 4]

example2 :: [R]
example2 = [Let 3 $ Value $ Var 0, Value $ Lit 4]

result2 :: R
result2 = Let 3 $ Value $ Sum [Var 0, Lit 4]

example3 :: [R]
example3 = [Let 3 $ Value $ Var 0, Let 4 $ Value $ Var 0]

result3 :: R
result3 = Let 3 $ Let 4 $ Value $ Sum [Var 1, Var 0]

example4 :: [R]
example4 = 
  [ Let 1 $ Let 3 $ Value $ Sum [Var 1, Var 0]
  , Let 2 $ Let 4 $ Value $ Sum [Var 0, Var 1]
  ]

result4 :: R
result4 = Let 1 $ Let 3 $ Let 2 $ Let 4 $ Value $ Sum [Sum [Var 3, Var 2], Sum [Var 0, Var 1]]
