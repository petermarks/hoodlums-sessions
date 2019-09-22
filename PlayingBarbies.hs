{-# language RecordWildCards, TypeFamilies, KindSignatures, InstanceSigs, RankNTypes #-}

module PlayingBarbies where

import Data.Functor.Identity
-- import Data.Barbie

type family HKT f a where
  HKT Identity a = a
  HKT f a = f a

data PersonB f
  = PersonB
      { name :: f String
      , age  :: f Int
      , relatives :: f [PersonB f]
      }

bmap :: Functor f => (forall a. f a -> g a) -> PersonB f -> PersonB g
bmap f PersonB{..} = PersonB
  { name = f name
  , age = f age
  , relatives = f (fmap (map (bmap f)) relatives)
  }

type Person = PersonB Identity
-- type PersonLens = PersonB Lens
-- type PersonIO = PersonB IO
type PersonOverrides = PersonB Maybe

getPerson :: PersonOverrides -> Maybe Person
getPerson PersonB{..} = do
  name <- name
  age <- age
  relatives <- relatives
  relatives' <- traverse getPerson relatives
  pure PersonB{name=Identity name, age=Identity age, relatives=Identity relatives'}