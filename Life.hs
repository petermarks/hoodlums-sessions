{-# LANGUAGE DeriveFoldable #-}

module Life where

import Data.Array
import Data.List
import Graphics.Gloss
import Data.Monoid
import Control.Applicative
import qualified Data.Foldable as F 

data Grid a = Grid (Array (Int,Int) a) (Int, Int)
  deriving F.Foldable

class Comonad w where
  extract :: w a -> a
  (=>>) :: w a -> (w a -> b) -> w b

instance Comonad Grid where 
  extract (Grid a p) = a ! p
  (Grid a p) =>> f   = Grid ( listArray (bounds a) . map (f . Grid a) $ indices a) p

main :: IO ()
main = do
  gridString <- readFile "grid.life"
  let grid = parseGrid gridString
  runGame grid

parseGrid :: String -> Grid Bool
parseGrid s = Grid ( listArray ((1,1), (width, height)) es) (1,1)
  where
    ls     = dropWhile ("!" `isPrefixOf`) $ lines s
    width  = length $ head ls
    height = length ls
    es     =  map (== 'O') $ concat $ transpose ls

runGame :: Grid Bool -> IO ()
runGame grid = simulate (InWindow "Life" (windowSize grid) (10,10))
                black 10 grid render step

size :: Grid a -> (Int, Int)
size (Grid a _) = snd (bounds a)

render :: Grid Bool -> Picture
render g = color red $ F.fold $ g =>> renderCell

gridIndex :: Grid a -> (Int, Int)
gridIndex (Grid _ p) = p

gridIndices :: Grid a -> Grid (Int, Int)
gridIndices g = g =>> gridIndex

renderCell :: Grid Bool -> Picture
renderCell g | alive = translate (fromIntegral $ x * 10 - w * 5 - 15)
                                 (fromIntegral $ y * (-10) + h * 5 + 15) $
                                 rectangleSolid 8 8
             | otherwise = mempty   
  where
    alive = extract g
    (x, y) = gridIndex g
    (w, h) = size g

windowSize :: Grid a -> (Int, Int)
-- windowSize g = (x * 10, y * 10)
--   where (x, y) = size g
windowSize _ = (500, 500)

rule :: Bool -> Int-> Bool
rule True  i = i == 2 || i == 3
rule False i = i == 3

moveGrid :: (Int, Int) -> Grid a -> Grid a
moveGrid (xx,yy) (Grid a (x,y)) = Grid a (x',y')
  where
    (w,h) = snd $ bounds a
    x' = (x + xx - 1) `mod` w + 1
    y' = (y + yy - 1) `mod` h + 1

neighbours :: Grid Bool -> Int
neighbours g = length . filter id $ bools
  where
    bools = map (\o -> extract $ moveGrid o g) offsets
    offsets = [(x,y) | x <- [(-1)..1], y <- [(-1)..1], (x,y) /= (0,0)]

step :: x -> Float -> Grid Bool -> Grid Bool
step _ _ g = g =>> (rule <$> extract <*> neighbours)
