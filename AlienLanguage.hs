module AlienLanguage where

import Text.Printf
import Data.Bits
import Data.Char

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
process dictionary = map (testPattern dictionary' . patternToBits)
  where
    dictionary' = map wordToBits dictionary

testPattern :: [[Int]] -> [Int] -> Int
testPattern dictionary pattern = length $ filter (found pattern) dictionary

found :: [Int] -> [Int] -> Bool
found pattern = notElem 0 . zipWith (.&.) pattern

charToBits :: Char -> Int
charToBits = bit . subtract (ord 'a') . ord

wordToBits :: String -> [Int]
wordToBits = map charToBits

patternToBits :: String -> [Int]
patternToBits [] = []
patternToBits ('(':cs) = let (alts, _:cs') = break (==')')cs in sum (map charToBits alts) : patternToBits cs'
patternToBits (c:cs) = charToBits c : patternToBits cs

output :: [Int] -> String
output = unlines . zipWith (printf "Case #%d: %d") [(1::Int)..]
