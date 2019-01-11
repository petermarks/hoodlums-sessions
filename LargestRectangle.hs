{-# LANGUAGE FlexibleInstances, UndecidableInstances, DuplicateRecordFields, RecordWildCards #-}

module Main where

import Control.Monad
--import Data.Array
--import Data.Bits
import Data.List hiding (subsequences)
--import Data.List.Split
--import Data.Set
import Debug.Trace
import System.Environment
import System.IO
--import System.IO.Unsafe

data State = State
  { sMax   :: Int
  , sStack :: [(Int, Int)]
  , sIndex :: Int
  }

initial :: State
initial = State 0 [] 0

-- Complete the largestRectangle function below.
largestRectangle :: [Int] -> Int
largestRectangle = sMax . flip step 0 . foldl' step initial

step :: State -> Int -> State
step state@State{sStack = [], ..} h = state{sStack = [(h, sIndex)], sIndex = sIndex + 1}
step state@State{sStack = (hp, i) : stack, ..} h = 
  let 
    state' = case compare h hp of
      EQ -> state
      GT -> state{sStack = (h, sIndex) : sStack state}
      LT -> unwind state i h
  in
    state'{sIndex = sIndex + 1}

unwind :: State -> Int -> Int -> State
unwind state@State{sStack = [], ..} i' h = state{sStack = [(h, i')]}
unwind state@State{sStack = (hp, i) : stack, ..} i' h = case compare h hp of
  EQ -> state
  GT -> state{sStack = (h, i') : sStack state}
  LT -> unwind state{sStack = stack, sMax = max sMax (hp * (sIndex - i))} i h


naiveLargestRectangle :: [Int] -> Int
naiveLargestRectangle = maximum . fmap (\hs -> length hs * minimum hs) . subsequences

subsequences :: [a] -> [[a]]
subsequences = tail . inits >=> init . tails

readMultipleLinesAsStringArray :: Int -> IO [String]
readMultipleLinesAsStringArray 0 = return []
readMultipleLinesAsStringArray n = do
    line <- getLine
    rest <- readMultipleLinesAsStringArray(n - 1)
    return (line : rest)

main :: IO()
main = do
    stdout <- getEnv "OUTPUT_PATH"
    fptr <- openFile stdout WriteMode

    n <- readLn :: IO Int

    hTemp <- getLine

    let h = fmap (read :: String -> Int) . words $ hTemp

    let result = largestRectangle h

    hPutStrLn fptr $ show result

    hFlush fptr
    hClose fptr
