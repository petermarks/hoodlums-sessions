module Registers where

import qualified Data.Map.Strict as M 
import Data.List

data CmpOp = OpEQ | OpLT | OpGT | OpLEQ | OpGEQ | OpNEQ deriving (Show)

readOp :: String -> CmpOp
readOp "==" = OpEQ
readOp "<"  = OpLT
readOp ">"  = OpGT
readOp "<=" = OpLEQ
readOp ">=" = OpGEQ
readOp "!=" = OpNEQ
readOp x    = error $ "Invalid comparison: " ++ x

newtype Reg = Reg String deriving (Eq, Ord, Show)

type Registers = M.Map Reg Int

data Instruction = Instruction
  { iTarget :: Reg
  , iDelta  :: Int
  , iCmpSrc :: Reg
  , iCmpOp  :: CmpOp
  , iCmpVal :: Int
  }
  deriving Show

parse :: String -> [Instruction]
parse = map (parseLine . words) . lines

parseLine ::[String] -> Instruction
parseLine [t, a, d, "if", s, o, v] = Instruction 
  { iTarget = Reg t
  , iDelta  = (if a == "dec" then negate else id) $ read d
  , iCmpSrc = Reg s
  , iCmpOp  = readOp o
  , iCmpVal = read v
  }
parseLine x = error $ "Malformed line: " ++ unwords x

run :: [Instruction] -> Int
run = snd . foldl' step (mempty, 0)

step :: (Registers, Int) -> Instruction -> (Registers, Int)
step (rs, m) (Instruction t d s o v) 
  | cmp o (M.findWithDefault 0 s rs) v = let rs' = M.insertWith (+) t d rs in (rs', max m (rs' M.! t))
  | otherwise                          = (rs, m)

cmp :: CmpOp -> Int -> Int -> Bool
cmp OpEQ = (==)
cmp OpLT = (<)
cmp OpGT = (>)
cmp OpLEQ = (<=)
cmp OpGEQ = (>=)
cmp OpNEQ = (/=)

main :: IO ()
main = do
  f <- readFile "instructions.txt"
  print $ run $ parse f