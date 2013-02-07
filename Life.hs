module Life where

import Data.Array
import Data.List

data Grid = Grid (Array (Int,Int) Bool)


main :: IO ()
main = do
  gridString <- readFile "grid.life"
  let grid = parseGrid gridString
  runGame grid

parseGrid :: String -> Grid
parseGrid s = Grid $ listArray ((0,0), (width-1, height-1)) es
  where
    ls     = dropWhile ("!" `isPrefixOf`) $ lines s
    width  = length $ head ls
    height = length ls
    es     = map (== 'O') $ concat ls

runGame :: Grid -> IO ()
runGame = undefined