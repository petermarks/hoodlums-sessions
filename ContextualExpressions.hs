{-# LANGUAGE GADTs, KindSignatures, DataKinds, StandaloneDeriving, TypeOperators #-}
module ContextualExpressions where

import Prelude hiding (sum)
import GHC.TypeLits

data Expr :: Nat -> * where
  Lit :: Double   -> Expr n
  Var :: Int      -> Expr n
  Sum :: [Expr n] -> Expr n

deriving instance Show (Expr n)

data R :: Nat -> * where
  Value :: Expr n -> R n
  Let   :: Double -> R (n + 1) -> R n 

deriving instance Show (R n)

shiftE :: Expr n -> Expr (n + 1)
shiftE (Lit x) = Lit x
shiftE (Var n) = Var $ n + 1
shiftE (Sum es) = Sum $ map shiftE es

promoteE :: Expr n -> Expr (n + 1)
promoteE (Lit x) = Lit x
promoteE (Var n) = Var n
promoteE (Sum es) = Sum $ map promoteE es

promoteR :: R n -> R (n + 1)
promoteR (Value e) = Value $ promoteE e
promoteR (Let x r) = Let x $ promoteR r

sum :: [R n] -> R n
sum = sum' [] 

sum' :: [Expr n] -> [R n] -> R n
sum' es [] = Value $ Sum $ reverse es
sum' es (Value e : rs) = sum' (e : es) rs
sum' es (Let x r : rs) = Let x $ sum' (map shiftE es) (r : map promoteR rs) 

example1 :: [R 0]
example1 = [Value $ Lit 3, Value $ Lit 4]

result1 :: R 0
result1 = Value $ Sum [Lit 3, Lit 4]

example2 :: [R 0]
example2 = [Let 3 $ Value $ Var 0, Value $ Lit 4]

result2 :: R 0
result2 = Let 3 $ Value $ Sum [Var 0, Lit 4]

example3 :: [R 0]
example3 = [Let 3 $ Value $ Var 0, Let 4 $ Value $ Var 0]

result3 :: R 0
result3 = Let 3 $ Let 4 $ Value $ Sum [Var 1, Var 0]

example4 :: [R 0]
example4 = 
  [ Let 1 $ Let 3 $ Value $ Sum [Var 1, Var 0]
  , Let 2 $ Let 4 $ Value $ Sum [Var 0, Var 1]
  ]

result4 :: R 0
result4 = Let 1 $ Let 3 $ Let 2 $ Let 4 $ Value $ Sum [Sum [Var 3, Var 2], Sum [Var 0, Var 1]]
