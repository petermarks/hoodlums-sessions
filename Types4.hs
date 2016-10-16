{-# LANGUAGE GeneralizedNewtypeDeriving, TypeOperators, TypeFamilies,
    FlexibleContexts #-}

module Types4 where

-- Derived from Connor McBride's WhatrTypes4 at 
-- https://github.com/pigworker/WhatRTypes4/blob/master/Types4.hs

import Data.Monoid
import Control.Applicative
import Control.Monad
import Control.Arrow
import Data.Char


newtype P thing = P {parse :: String -> [(thing, String)]} deriving Monoid

instance Functor P where
  fmap f (P p) = P $ fmap (first f) . p -- P $ \s -> fmap (first f) $ p s

instance Applicative P where
  pure x = P $ \s -> [(x, s)]
  (P pf) <*> (P pv) = P $ \s -> [(f v, s'') | (f, s') <- pf s, (v, s'') <- pv s']

-- instance Monad P where
--   return x = P $ \ s -> [(x, s)]
--   P af >>= k = P $ \ s -> do
--     (a, s) <- af s
--     parse (k a) s

instance Alternative P where  -- (what if P used Maybe?)
  empty = mempty
  (<|>) = mappend

eat :: (Char -> Bool) -> P Char
eat p = P $ \s -> case s of
  (c : s) | p c -> [(c, s)]
  _ -> []

type Cell = Maybe Int

pcell :: P Cell
pcell = many (eat isSpace) *> (Just . digitToInt <$> eat isDigit <|> Nothing <$ eat (== '.')) 




newtype         I x = I x                 deriving (Show, Functor)
newtype       K a x = K a                 deriving (Show, Functor)
data    (f :*: g) x = f x :*: g x         deriving Show
data    (f :+: g) x = L (f x) | R (g x)   deriving Show
newtype (f :.: g) x = C {unC :: f (g x)}  deriving Show

instance (Functor f, Functor g) => Functor (f :*: g) where
  fmap k (fx :*: gx) = fmap k fx :*: fmap k gx

instance (Functor f, Functor g) => Functor (f :+: g) where
  fmap k (L fx) = L (fmap k fx)
  fmap k (R gx) = R (fmap k gx)

instance (Functor f, Functor g) => Functor (f :.: g) where
  fmap k (C fgx) = C (fmap (fmap k) fgx)

instance Applicative I where
  pure = I
  I f <*> I s = I (f s)

instance Monoid a => Applicative (K a) where
  pure x = K mempty
  K f <*> K s = K (mappend f s)

instance (Applicative f, Applicative g) => Applicative (f :*: g) where
  pure x = pure x :*: pure x
  (ff :*: gf) <*> (fs :*: gs) = (ff <*> fs) :*: (gf <*> gs)

instance (Applicative f, Applicative g) => Applicative (f :.: g) where
  pure = C . pure . pure
  C fgf <*> C fgs = C $ liftA2 (<*>) fgf fgs

