module Trains where

import Data.Char
import Data.List
import qualified Data.Set as S
import Text.Printf

main :: IO ()
main = interact go

go :: String -> String
go = output . map process . parse

parse :: String -> [[String]]
parse = map words . filter (isAlpha . head) . lines

output :: [Int] -> String
output = unlines . zipWith (printf "Case #%d: %d") [(1::Int)..]

process :: [String] -> Int
process trains= length
  [ ()
  | ordering <- permutations trains
  , let combined = concat ordering
  , numUnique == length (group combined)
  ]
  where
    numUnique = S.size (S.fromList (concat trains))