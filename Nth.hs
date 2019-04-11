{-# LANGUAGE DataKinds, KindSignatures, GADTs #-}

module Nth where

data Nat = Z | S Nat

data Vect :: Nat -> * -> * where
  Nil :: Vect Z a
  (:::) :: a -> Vect n a -> Vect (S n) a

data Fin :: Nat -> * where
  FZ :: Fin (S n)
  FS :: Fin n -> Fin (S n)


data LTE :: Nat -> Nat -> * where
  LTEZ :: LTE Z n
  LTES :: LTE m n -> LTE (S m) (S n)

nth :: LTE n m -> Fin n -> Vect m a -> a
nth (LTES _) FZ (x ::: _) = x
nth (LTES p) (FS k) (_ ::: xs) = nth p k xs