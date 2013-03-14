module Life where

import Prelude hiding (concat)
import Control.Applicative
import Data.Foldable
import Data.List hiding (concat)
import Data.Monoid
import Graphics.Gloss

import Grid

main :: IO ()
main = do
  gridString <- readFile "grid.life"
  let grid = parseGrid gridString
  runGame grid

parseGrid :: String -> Grid Bool
parseGrid s = mkGrid width height es
  where
    ls     = dropWhile ("!" `isPrefixOf`) $ lines s
    width  = length $ head ls
    height = length ls
    es     = map (== 'O') $ concat ls

runGame :: Grid Bool -> IO ()
runGame grid = 
  simulate (InWindow "Life" (windowSize grid) (10,10)) black 10 grid render step

render :: Grid Bool -> Picture
render g = color red $ fold $ g =>> renderCell

gridIndices :: Grid a -> Grid (Int, Int)
gridIndices g = g =>> gridIndex

renderCell :: Grid Bool -> Picture
renderCell g | alive = translate xx yy $ rectangleSolid 8 8
             | otherwise = mempty   
  where
    alive  = extract g
    (x, y) = gridIndex g
    xx     = fromIntegral $ x * 10    - gridWidth  g * 5 + 5
    yy     = fromIntegral $ y * (-10) + gridHeight g * 5 - 5


windowSize :: Grid a -> (Int, Int)
windowSize g = (gridWidth g * 10, gridHeight g * 10)

rule :: Bool -> Int-> Bool
rule True  i = i == 2 || i == 3
rule False i = i == 3

neighbours :: Grid Bool -> Int
neighbours g = length . filter id $ bools
  where
    bools   = map (\o -> extract $ gridMove o g) offsets
    offsets = [(x,y) | x <- [(-1)..1], y <- [(-1)..1], (x,y) /= (0,0)]

step :: x -> Float -> Grid Bool -> Grid Bool
step _ _ g = g =>> (rule <$> extract <*> neighbours)
