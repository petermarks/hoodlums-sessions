module ListUpdate where

import Test.QuickCheck
import Data.List
import Data.Function
import Data.Ord

data Operation = Delete | SetTrue
  deriving (Eq, Enum, Bounded, Show)

instance Arbitrary Operation where
  arbitrary = arbitraryBoundedEnum

original :: [(Int, Bool)]
original = 
  [ (1000, False)
  , (1001, True)
  , (1004, False)
  , (1006, False)
  ]

updates :: [(Int, Operation)]
updates =
  [ (1003, Delete)
  , (1004, Delete)
  , (1006, SetTrue)]

processNaive :: [(Int, Operation)] -> [(Int, Bool)] -> [(Int, Bool)]
processNaive us xs = foldr processOne xs us

processOne :: (Int, Operation) -> [(Int, Bool)] -> [(Int, Bool)]
processOne (i, Delete)  = filter ((/= i) . fst)
processOne (i, SetTrue) = map (\(j, b) -> if i == j then (j, True) else (j, b))

process :: [(Int, Operation)] -> [(Int, Bool)] -> [(Int, Bool)]
process _                []          = []
process []               xs          = xs
process us@((i, op):us') xs@((j, b):xs') = case i `compare` j of
  LT -> process us' xs
  GT -> (j, b) : process us xs'
  EQ -> case op of
    Delete  -> process us' xs'
    SetTrue -> (j, True) : process us' xs'

prop_process_matches_naive :: [(Int, Operation)] -> [(Int, Bool)] -> Bool
prop_process_matches_naive us xs = processNaive us' xs' == process us' xs'
  where
    us' = nubBy ((==) `on` fst) . sortBy (comparing fst) $ us
    xs' = nubBy ((==) `on` fst) . sortBy (comparing fst) $ xs

main :: IO ()
main = quickCheck prop_process_matches_naive