{-# language TypeFamilies #-}
{-# language UndecidableInstances #-}
{-# language MultiParamTypeClasses #-}
{-# language OverlappingInstances #-}
{-# language FlexibleInstances #-}

module Uncurry where

curry2 :: ((a, b) -> c) -> a -> b -> c
curry2 f a b = f (a, b)

uncurry2 :: (a -> b -> c) -> (a, b) -> c
uncurry2 f (a, b) = f a b

-- uncurry2 :: (a -> b -> (c -> d)) -> (a, b) -> (c -> d)
-- uncurry2 f :: (a, b) -> c -> d
-- uncurry2 :: ((a, b) -> c -> d) -> ((a, b), c) -> d

uncurry3 :: (a -> b -> c -> d) -> ((a, b), c) -> d
uncurry3 = uncurry2 . uncurry2

type Uncurry4 a b c d e = (a -> b -> c -> d -> e) -> (((a, b), c), d) -> e

type family Domain a where Domain (a -> b) = a
type family Codomain a where Codomain (a -> b) = b

uncurry4 :: Uncurry4 a b c d e
uncurry4 = uncurry2 . uncurry3

class Uncurry a f where
  type Uncurried a f
  uncurryN :: (a -> f) -> Uncurried a f

instance (Uncurry b c) => Uncurry a (b -> c) where
  type Uncurried a (b -> c) = Uncurried (a, b) c
  uncurryN = uncurry2 . uncurryN

instance Uncurry a b where
  type Uncurried a b = a -> b
  uncurryN = id

-- type family Uncurried a b where
--   Uncurried a (b -> c)  = Uncurried (a, b) c
--   Uncurried a b = a -> b