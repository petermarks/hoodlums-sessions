module Trains where

import Control.Monad
import Data.Char
import Data.List
import Data.Maybe
import qualified Data.Set as S
import Text.Printf

data Train = Train Char (S.Set Char) Char
  deriving (Show)

main :: IO ()
main = interact go

go :: String -> String
go = output . map process . parse

parse :: String -> [[String]]
parse = map words . filter (isAlpha . head) . lines

output :: [Int] -> String
output = unlines . zipWith (printf "Case #%d: %d") [(1::Int)..]

process :: [String] -> Int
process = maybe 0 (length . mapMaybe check . permutations) . mapM train
  where
    check :: [Train] -> Maybe Train
    check (t:ts) = foldM couple t ts
    check []     = Nothing

train :: String -> Maybe Train
train []     = Nothing
train [c]    = Just $ Train c (S.singleton c) c
train (c:cs) = train cs >>= coupleCar
  where
    coupleCar tt@(Train l s r) 
      | l == c         = Just tt
      | c `S.member` s = Nothing
      | otherwise      = Just $ Train c (S.insert c s) r

couple :: Train -> Train -> Maybe Train
couple (Train l1 s1 r1) (Train l2 s2 r2)
  | S.null s || (r1 == l2 && s == S.singleton r1) = Just $ Train l1 (s1 `S.union` s2) r2
  | otherwise                                     = Nothing
  where
    s = s1 `S.intersection` s2