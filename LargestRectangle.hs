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
  }

initial :: State
initial = State 0 [(0,0)]

-- Complete the largestRectangle function below.
largestRectangle :: [Int] -> Int
largestRectangle = sMax . foldl' step initial . zip [0..] . (++ [0])

step :: State -> (Int, Int) -> State
step state (i,h) = step' state i i h

step' :: State -> Int -> Int -> Int -> State
step' state@State{sStack = (hp, start) : stack} end start' h = case compare h hp of
  EQ -> state
  GT -> state{sStack = (h, start') : sStack state}
  LT -> step' State{sStack = stack, sMax = max (sMax state) (hp * (end - start))} end start h


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
