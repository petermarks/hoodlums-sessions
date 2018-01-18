module RecursiveCircus where

import Data.Set (Set)
import qualified Data.Set as S
--import Data.Monoid
--import Data.Foldable
--import Control.Applicative
import Control.Arrow

type Node = (String, Set String)

type Tower = (Set String, Set String)

solve :: String -> String
solve = pickRoot . setify . map splitUp . lines

splitUp :: String -> Node
splitUp = f . words . filter (/= ',')
  where
    f :: [String] -> Node
    f (p : _ : _ : cs) = (p, S.fromList cs)
    f (p : _) = (p, mempty)
    f l = error $ "invalid line: " ++ unwords l

setify :: [Node] -> Tower
setify = foldMap (first S.singleton)

pickRoot :: Tower -> String
pickRoot = S.findMin . uncurry S.difference

main :: IO ()
main = do
  f <- readFile "input.txt"
  let ans = solve f
  putStrLn ans 