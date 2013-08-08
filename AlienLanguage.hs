module AlienLanguage where

main :: IO ()
main = interact $ output . uncurry process . parse

parse :: String -> ([String], [String])
parse content =
  let (parameters : body) = lines content
      [_, dictionarySize, _] = map read $ words parameters
  in splitAt dictionarySize body

process :: [String] -> [String] -> [Int]
process = undefined

output :: [Int] -> String
output = undefined