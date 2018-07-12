module Day9 (main) where

import Data.List

removeBangs :: String -> String
removeBangs [] = []
removeBangs ('!' : _ : xs) = removeBangs xs
removeBangs (x : xs) = x : removeBangs xs

removeDoubleBangs' :: String -> String
removeDoubleBangs' = foldr go []
  where
    go '!' ('!' : xs) = xs
    go x xs = x : xs

removeBangs' :: String -> String
removeBangs' = foldr go []
  where
    go '!' (_ : xs) = xs
    go x xs = x : xs

-- Doesn't stream
removeBangs'' :: String -> String
removeBangs'' = snd . foldr go (False, [])
  where
    go '!' (False, xs) = (True, xs)
    go '!' (True, xs) = (False, xs)
    go x (True, _ : xs) = (False, x : xs)
    go x (False, xs) = (False, x : xs)

removeGarbage :: String -> String
removeGarbage [] = []
removeGarbage ('<' : xs) = removeGarbage $ drop 1 $ dropWhile (/= '>') xs
removeGarbage (x : xs) = x : removeGarbage xs

scoreGroups :: String -> Int
scoreGroups = go 0 0
  where
    go depth score [] = score
    go depth score ('{' : xs) = go (depth + 1) score xs
    go depth score ('}' : xs) = go (depth - 1) (score + depth) xs
    go depth score (_ : xs) = go depth score xs 

process :: String -> Int
process = scoreGroups . removeGarbage . removeBangs''

main :: IO ()
main = interact $ show . process