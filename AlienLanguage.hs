module AlienLanguage where

import Text.Printf
import Text.Regex.Posix

main :: IO ()
main = interact go

go :: String -> String
go = output . uncurry process . parse

parse :: String -> ([String], [String])
parse content =
  let (parameters : body) = lines content
      [_, dictionarySize, _] = map read $ words parameters
  in splitAt dictionarySize body

process :: [String] -> [String] -> [Int]
process dictionary = map (testPattern dictionary)

testPattern :: [String] -> String -> Int
testPattern dictionary pattern = length $ filter (=~ pattern') dictionary
  where
    pattern' = map f pattern
    f '(' = '['
    f ')' = ']'
    f c   = c
    

output :: [Int] -> String
output = unlines . zipWith (printf "Case #%d: %d") [(1::Int)..]
