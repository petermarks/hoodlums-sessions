module RecursiveCircus where

import Data.Set (Set)
import qualified Data.Set as S
import Data.Map (Map)
import qualified Data.Map as M
import Data.List (nub)

type Node = (String, Int, Set String)

type Tower = (Set String, Set String)

type WeightMap = Map String Int

solveRoot :: [Node] -> String
solveRoot = pickRoot . setify 

parse :: String -> [Node]
parse = map splitUp . lines

splitUp :: String -> Node
splitUp = f . words . filter (/= ',')
  where
    f :: [String] -> Node
    f (p : w : _ : cs) = (p, fst $ head $ reads $ tail w, S.fromList cs)
    f (p : w : _) = (p, fst $ head $ reads $ tail w, mempty)
    f l = error $ "invalid line: " ++ unwords l

setify :: [Node] -> Tower
setify = foldMap (\(p, _, cs) -> (S.singleton p, cs))

pickRoot :: Tower -> String
pickRoot = S.findMin . uncurry S.difference

buildWeightMap :: [Node] -> WeightMap
buildWeightMap ns = wm
  where
    wm = M.fromList $ map f ns
    f (p, w, cs) = (p, w + sum (fmap (wm M.!) (S.toList cs)))

isImbalanced :: WeightMap -> Node -> Bool
isImbalanced wm (_, _, cs) = (> 1) $ length $ nub $ map (wm M.!) $ S.toList cs

main :: IO ()
main = do
  f <- readFile "input.txt"
  let parsed = parse f
  let root = solveRoot parsed
  let wm = buildWeightMap parsed
  let imbalances = filter (isImbalanced wm) parsed
  putStrLn root
  print $ wm M.! root
  print imbalances