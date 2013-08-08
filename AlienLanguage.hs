module AlienLanguage where

import Text.Printf

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
process dictionary patterns = [ length dictionary, length patterns ]

output :: [Int] -> String
output = unlines . zipWith (printf "Case #%d: %d") [(1::Int)..]
