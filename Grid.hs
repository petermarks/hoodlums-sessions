{-# LANGUAGE DeriveFoldable, RecordWildCards #-}

module Grid
  ( Grid         -- Keep grid abstract by not exporting constructors
  , Comonad(..)
  , mkGrid
  , gridWidth
  , gridHeight
  , gridIndex
  , gridMove
  ) where

import Prelude hiding (concat)
import Data.Array
import Data.Foldable

data Grid a = Grid
  { gridWidth  :: Int
  , gridHeight :: Int
  , gridData   :: Array Int a
  , gridIndex  :: (Int, Int)
  }
  deriving Foldable

instance Functor Grid where
  fmap f g = g{gridData = fmap f $ gridData g}

class Comonad w where
  extract :: w a -> a
  (=>>)   :: w a -> (w a -> b) -> w b

instance Comonad Grid where 
  extract Grid{..} = gridData ! (snd gridIndex * gridWidth + fst gridIndex)
  g@Grid{..} =>> f = 
    mkGrid gridWidth gridHeight
      [f $ setIndex (x, y) g | y <- [0..gridHeight - 1], x <- [0..gridWidth - 1]]

mkGrid :: Int -> Int -> [a] -> Grid a
mkGrid w h es = Grid w h (listArray (0, w * h - 1) es) (0, 0)

setIndex :: (Int, Int) -> Grid a -> Grid a
setIndex i g = g{gridIndex = i}

gridMove :: (Int, Int) -> Grid a -> Grid a
gridMove (dx, dy) g@Grid{..} = setIndex (x', y') g
  where
    (x, y) = gridIndex
    x'     = (x + dx) `mod` gridWidth
    y'     = (y + dy) `mod` gridHeight
