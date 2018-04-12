module SpiralMemory where

findRingBF :: Int -> Int
findRingBF target = head $ dropWhile (\x -> x * x < target) [1,3..]

findRing :: Int -> Int
findRing = (\i -> if even i then i + 1 else i) . ceiling . (sqrt :: Double -> Double) . fromIntegral

manhattenDistance :: Int -> Int
manhattenDistance 1 = 0
manhattenDistance target = ringSteps + offsetSteps
  where
    ring = findRing target
    ringSteps = (ring - 1) `div` 2
    diff = ring * ring - target
    offsetSteps = abs $ diff `mod` (ring - 1) - ringSteps
