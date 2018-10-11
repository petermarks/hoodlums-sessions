{-# LANGUAGE TemplateHaskell #-}

module Lib (

) where

import Data.Function
import Data.Maybe

import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

lcsNaive :: Eq a => [a] -> [a] -> [a]
lcsNaive [] _ = []
lcsNaive _ [] = []
lcsNaive xs@(x:xs') ys@(y:ys') 
  | x == y = x : lcsNaive xs' ys'
  | otherwise = longest (lcsNaive xs ys') (lcsNaive xs' ys)

longest :: [a] -> [a] -> [a]
longest xs ys
  | length xs >= length ys = xs
  | otherwise = ys

lcsUnfixed :: Eq a => ([a] -> [a] -> [a]) -> [a] -> [a] -> [a]
lcsUnfixed _ [] _ = []
lcsUnfixed _ _ [] = []
lcsUnfixed r xs@(x:xs') ys@(y:ys') 
  | x == y = x : r xs' ys'
  | otherwise = longest (r xs ys') (r xs' ys)

lcsFixed :: Eq a => [a] -> [a] -> [a]
lcsFixed = fix lcsUnfixed
  
lcsPoints :: Eq a => [a] -> [a] -> [a] -> a -> a -> [a]
lcsPoints right diag down x y
  | x == y = x : diag
  | otherwise = longest down right

lcsTopDown :: Eq a => [a] -> [a] -> [a]
lcsTopDown xs ys = go xs ys
  where
    go [] _ = []
    go _ [] = []
    go xs@(x:xs') ys@(y:ys') = lcsPoints (go xs' ys) (go xs' ys') (go xs ys') x y
 
lcsBottomUp :: Eq a => [a] -> [a] -> [a]
lcsBottomUp xs ys = fromMaybe [] $ listToMaybe $ foldr goRow (map (const []) xs) ys
  where
    goRow y row = fst $ foldr goCol ([], ([], [])) (zip row xs)
      where
        goCol (down, x) (acc, (right, diag)) = (r : acc, (r, down))
          where
            r = lcsPoints right diag down x y
  
prop_simple :: Property
prop_simple =
  property $ do
    assert $ lcsNaive "gac" "agcat" `elem` ["ga", "ac", "gc"]

prop_idempotent :: Property
prop_idempotent =
  property $ do
    xs <- forAll $ Gen.list (Range.linear 0 10) Gen.alpha
    ys <- forAll $ Gen.list (Range.linear 0 10) Gen.alpha
    let r = lcsNaive xs ys
    lcsNaive xs (lcsNaive xs ys) === r
    lcsNaive ys (lcsNaive xs ys) === r

prop_subsequence :: Property
prop_subsequence =
  property $ do
    xs <- forAll $ Gen.list (Range.linear 0 10) Gen.alpha
    ys <- forAll $ Gen.list (Range.linear 0 10) Gen.alpha
    let r = lcsNaive xs ys
    assert $ isSubsequence r xs
    assert $ isSubsequence r ys

isSubsequence :: Eq a => [a] -> [a] -> Bool
isSubsequence [] _ = True
isSubsequence _ [] = False
isSubsequence xs@(x:xs') (y:ys') 
  | x == y = isSubsequence xs' ys'
  | otherwise = isSubsequence xs ys'

prop_topDown_equals_naive :: Property
prop_topDown_equals_naive =
  property $ do
    xs <- forAll $ Gen.list (Range.linear 0 10) Gen.alpha
    ys <- forAll $ Gen.list (Range.linear 0 10) Gen.alpha
    lcsNaive xs ys === lcsTopDown xs ys

prop_topDown_equals_bottomUp :: Property
prop_topDown_equals_bottomUp =
  property $ do
    xs <- forAll $ Gen.list (Range.linear 0 10) Gen.alpha
    ys <- forAll $ Gen.list (Range.linear 0 10) Gen.alpha
    lcsBottomUp xs ys === lcsTopDown xs ys

tests :: IO Bool
tests =
  checkParallel $$(discover)