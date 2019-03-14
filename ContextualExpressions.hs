{-# LANGUAGE GADTs, KindSignatures, DataKinds, StandaloneDeriving, TypeOperators #-}
module ContextualExpressions where

import Prelude hiding (sum)
import GHC.TypeLits

data Fin :: Nat -> * where
  Z :: Fin (n + 1)
  S :: Fin n -> Fin (n + 1)

deriving instance Show (Fin n)

data Expr :: Nat -> * where
  Lit :: Double   -> Expr n
  Var :: Fin n      -> Expr n
  Sum :: [Expr n] -> Expr n

deriving instance Show (Expr n)

data R :: Nat -> * where
  Value :: Expr n -> R n
  Let   :: Double -> R (n + 1) -> R n 

deriving instance Show (R n)

shiftE :: Expr n -> Expr (n + 1)
shiftE (Lit x) = Lit x
shiftE (Var n) = Var $ S n
shiftE (Sum es) = Sum $ map shiftE es

promoteF :: Fin n -> Fin (n + 1)
promoteF Z = Z
promoteF (S n) = S $ promoteF n

promoteE :: Expr n -> Expr (n + 1)
promoteE (Lit x) = Lit x
promoteE (Var n) = Var $ promoteF n
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
example2 = [Let 3 $ Value $ Var f0, Value $ Lit 4]

result2 :: R 0
result2 = Let 3 $ Value $ Sum [Var f0, Lit 4]

example3 :: [R 0]
example3 = [Let 3 $ Value $ Var f0, Let 4 $ Value $ Var f0]

result3 :: R 0
result3 = Let 3 $ Let 4 $ Value $ Sum [Var f1, Var f0]

example4 :: [R 0]
example4 = 
  [ Let 1 $ Let 3 $ Value $ Sum [Var f1, Var f0]
  , Let 2 $ Let 4 $ Value $ Sum [Var f0, Var f1]
  ]

result4 :: R 0
result4 = Let 1 $ Let 3 $ Let 2 $ Let 4 $ Value $ Sum [Sum [Var f3, Var f2], Sum [Var f0, Var f1]]

f0 = Z
f1 = S f0
f2 = S f1
f3 = S f2
f4 = S f3
f5 = S f4
f6 = S f5
f7 = S f6
f8 = S f7
f9 = S f8