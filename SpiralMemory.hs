module SpiralMemory where

findRingBF :: Int -> Int
findRingBF target = head $ dropWhile (\x -> x * x < target) [1,3..]

findRing :: Int -> Int
findRing = (`div` 2) . ceiling . (sqrt :: Double -> Double) . fromIntegral

manhattenDistance :: Int -> Int
manhattenDistance 1 = 0
manhattenDistance target = ring + offset
  where
    ring = findRing target
    ringLength = ring * 2 + 1
    diff = ringLength * ringLength - target
    offset = abs $ diff `mod` (ring * 2) - ring
