module Nono where

import Data.List

data Game = Game Int [Int]

testGame :: Game
testGame = Game 5 [2, 1]

naiveSolve :: Game -> [[Bool]]
naiveSolve (Game size blocks) = nub . filter (check blocks) . permutations $ makeList nts (size - nts)
  where
    nts = sum blocks

dump :: [[Bool]] -> IO ()
dump = putStrLn . unlines . map (map dumpChar)

dumpChar :: Bool -> Char
dumpChar True = '#'
dumpChar False = '.'

makeList :: Int -> Int -> [Bool]
makeList nts nfs = replicate nts True ++ replicate nfs False

-- assumes correct length and number of trues
check :: [Int] -> [Bool] -> Bool
check bs = (== bs) . map length . filter head . group

solve :: Game -> [[Bool]]
solve (Game l bs) = solve' l bs

solve' :: Int -> [Int] -> [[Bool]]
solve' l _ | l < 0     = []
solve' l [] = [replicate l False]
solve' l [b] | l == b = [replicate l True]
solve' l bs@(b : bs') =
  mapOnto (replicateOnto b True . (False :)) (solve' (l - b - 1) bs') $
  map (False :) (solve' (l - 1) bs)

replicateOnto :: Int -> a -> [a] -> [a]
replicateOnto 0 _ as = as
replicateOnto n a as = a : replicateOnto (n - 1) a as

--replicateOnto :: Int -> a -> [a] -> [a]
--replicateOnto 0 _ as = as
--replicateOnto n a as = replicateOnto (n - 1) a (a : as)

mapOnto :: (a -> b) -> [a] -> [b] -> [b]
mapOnto _ [] bs = bs
mapOnto f (x:xs) bs = f x : mapOnto f xs bs

main :: IO ()
main = print $ length $ solve (Game 250 [1,2,3,4])