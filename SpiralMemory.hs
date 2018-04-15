{-# language NamedFieldPuns #-}

module SpiralMemory where

import Control.Monad
import Control.Monad.Loops -- package monad-loops
import Control.Monad.ST
import Data.Monoid
import qualified Data.Vector.Unboxed.Mutable as V -- package vector
-- import Debug.Trace


------------------------------------------------------------------------
-- Manhatten Distance

findRing :: Int -> Int
findRing = (`div` 2) . ceiling . (sqrt :: Double -> Double) . fromIntegral

manhattenDistance :: Int -> Int
manhattenDistance 1 = 0
manhattenDistance target = ring + offset
  where
    ring = findRing target
    ringLength = ring * 2 + 1
    diff = ringLength * ringLength - target
    offset = abs $ diff `mod` (ring * 2) - ring



------------------------------------------------------------------------
-- Vec2

data Vec2 = Vec2 Int Int deriving (Show, Eq)

vrotc :: Vec2 -> Vec2
vrotc (Vec2 x y) = Vec2 y (-x)

vrota :: Vec2 -> Vec2
vrota (Vec2 x y) = Vec2 (-y) x

vinv :: Vec2 -> Vec2
vinv (Vec2 x y) = Vec2 (-x) (-y)

vadd :: Vec2 -> Vec2 -> Vec2
vadd (Vec2 x1 y1) (Vec2 x2 y2) = Vec2 (x1 + x2) (y1 + y2)

instance Monoid Vec2 where
  mempty = Vec2 0 0
  mappend = vadd



------------------------------------------------------------------------
-- Memory

data Memory s = Memory
  { mMem :: V.STVector s Int
  , mRing :: Int
  , mRingLength :: Int
  }

initMemory :: Int -> ST s (Memory s)
initMemory ring = do
  let ringLength = ring * 2 + 1
  mem <- V.new (ringLength * ringLength)
  return $ Memory mem ring ringLength 

pos2ix :: Vec2 -> Memory s -> Int
pos2ix (Vec2 x y) Memory{mRing, mRingLength} = (y + mRing) * mRingLength + x + mRing

mRead :: Vec2 -> Memory s -> ST s Int
mRead pos mem = V.read (mMem mem) (pos2ix pos mem) 

mWrite :: Vec2 -> Int -> Memory s -> ST s ()
mWrite pos val mem = V.write (mMem mem) (pos2ix pos mem) val



------------------------------------------------------------------------
-- Cursor

data Cursor = Cursor
  { pos :: Vec2
  , inward :: Vec2
  , around :: Vec2
  }

-- Cursor starts in the middle pointing downwards as it will turn left on the
-- first move.
initCursor :: Cursor
initCursor = Cursor (Vec2 0 0) (Vec2 1 0) (Vec2 0 (-1))

-- Move to the next cell in the spiral by following the left wall of non-zero
-- cells.
move :: Memory s -> Cursor -> ST s Cursor
move mem c@Cursor{pos, inward, around} = do
  left <- mRead (pos <> inward) mem 
  let
    turn = left == 0
    around' = if turn then inward else around
    inward' = if turn then vrota inward else inward
    pos' = pos <> around'  
  return $ Cursor pos' inward' around'



------------------------------------------------------------------------
-- Sum to target

sumToTarget :: Int -> Int
sumToTarget target = runST $ do
  -- Ensure mem is big enough to contain the target cell and have empty cells
  -- around the outside. This is very space inefficient, but I can't think of a
  -- way to find a lower upper bound.
  let ring = max 2 $ findRing target
  mem <- initMemory ring
  mWrite (Vec2 0 0) 1 mem
  (val, _) <- iterateUntilM ((>= target) . fst) (step mem) (1, initCursor)
  return val

step :: Memory s -> (Int, Cursor) -> ST s (Int, Cursor)
step mem (_, cursor) = do
  cursor'@Cursor{pos, inward, around} <- move mem cursor
  let left = pos <> inward
  val <- sumCells mem [left <> around, left, left <> vinv around, pos <> vinv around]
  mWrite pos val mem
  -- trace (show val ++ ' ' : show pos) $ return (val, cursor')
  return (val, cursor')

sumCells :: Memory s -> [Vec2] -> ST s Int
sumCells mem = foldM (\acc pos -> (+ acc) <$> mRead pos mem) 0
