{-# LANGUAGE DeriveFoldable #-}

module Grid
  ( Grid         -- Keep grid abstract by not exporting constructors
  , Comonad(..)
  , parseGrid
  , size
  , gridIndex
  , gridMove
  ) where

import Prelude hiding (concat)
import Data.Array
import Data.Foldable
import Data.List hiding (concat)

data Grid a = Grid (Array (Int,Int) a) (Int, Int)
  deriving Foldable

class Comonad w where
  extract :: w a -> a
  (=>>)   :: w a -> (w a -> b) -> w b

instance Comonad Grid where 
  extract (Grid a p) = a ! p
  (Grid a p) =>> f   = Grid ( listArray (bounds a) . map (f . Grid a) $ indices a) p

parseGrid :: String -> Grid Bool
parseGrid s = Grid ( listArray ((1, 1), (width, height)) es) (1, 1)
  where
    ls     = dropWhile ("!" `isPrefixOf`) $ lines s
    width  = length $ head ls
    height = length ls
    es     = map (== 'O') $ concat $ transpose ls

size :: Grid a -> (Int, Int)
size (Grid a _) = snd (bounds a)

gridIndex :: Grid a -> (Int, Int)
gridIndex (Grid _ p) = p

gridMove :: (Int, Int) -> Grid a -> Grid a
gridMove (dx, dy) (Grid a (x, y)) = Grid a (x', y')
  where
    (w, h) = snd $ bounds a
    x'     = (x + dx - 1) `mod` w + 1
    y'     = (y + dy - 1) `mod` h + 1
