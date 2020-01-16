{-# language NamedFieldPuns #-}

module ProgramAlarm where

import qualified Data.IntMap as IM
import Data.IntMap (IntMap)
import Control.Category hiding ((.))

data State = State {pc :: Int, mem :: IntMap Int}
  deriving (Show)

load :: [Int] -> State
load xs = State 0 (IM.fromAscList $ zip [0..] xs)

input :: Int -> Int -> State -> State
input n v s@State{mem} = s{mem = IM.insert 1 n $ IM.insert 2 v mem}

run :: State -> State
run s@State{pc, mem} = if mem IM.! pc == 99 then s else run (step s)

step :: State -> State
step s@State{pc, mem} = case mem IM.! pc of
  1 -> op (+) s
  2 -> op (*) s
  _ -> error "Invalid instruction"

op :: (Int -> Int -> Int) -> State -> State
op f State{pc, mem} = State (pc + 4) mem'
  where
    a = mem IM.! (mem IM.! (pc + 1))
    b = mem IM.! (mem IM.! (pc + 2))
    o = mem IM.! (pc + 3)
    mem' = IM.insert o (f a b) mem

dump :: State -> Int
dump State{mem} = mem IM.! 0

process :: Int -> Int -> [Int] -> Int
process n v = load >>> input n v >>> run >>> dump

exampleInput :: [Int]
exampleInput = [1,1,1,4,99,5,6,0,99]

exampleOutput :: [Int]
exampleOutput = [30,1,1,4,2,5,6,0,99]

prog :: [Int]
prog = [1,0,0,3,1,1,2,3,1,3,4,3,1,5,0,3,2,13,1,19,1,6,19,23,2,6,23,27,1,5,27,31,2,31,9,35,1,35,5,39,1,39,5,43,1,43,10,47,2,6,47,51,1,51,5,55,2,55,6,59,1,5,59,63,2,63,6,67,1,5,67,71,1,71,6,75,2,75,10,79,1,79,5,83,2,83,6,87,1,87,5,91,2,9,91,95,1,95,6,99,2,9,99,103,2,9,103,107,1,5,107,111,1,111,5,115,1,115,13,119,1,13,119,123,2,6,123,127,1,5,127,131,1,9,131,135,1,135,9,139,2,139,6,143,1,143,5,147,2,147,6,151,1,5,151,155,2,6,155,159,1,159,2,163,1,9,163,0,99,2,0,14,0]

search :: Int
search = head [100 * n + v | n <- [0..99], v <- [0..99], process n v prog == 19690720]