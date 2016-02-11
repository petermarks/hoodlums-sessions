module Evaluator where

import Prelude hiding (sum)

class ExprSym e where
  litI :: Int -> e
  litL :: [e] -> e
  add :: e -> e -> e
  sum :: e -> e

data Value = VInt Int | VList [Value] deriving Show

instance ExprSym Value where
  litI = VInt
  litL = VList
  add (VInt a) (VInt b) = VInt $ a + b
  add _ _ = error "Expected Ints"
  sum (VList []) = VInt 0
  sum (VList (VInt x : xs)) = add (VInt x) (sum $ VList xs)
  sum _ = error "Expected list of ints"

evaluate :: Value -> Value
evaluate = id