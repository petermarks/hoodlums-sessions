module AlienLanguage where

main :: IO ()
main = interact $ output . uncurry process . parse

parse :: String -> ([String], [String])
parse = undefined

process :: [String] -> [String] -> [Int]
process = undefined

output :: [Int] -> String
output = undefined