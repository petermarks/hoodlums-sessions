module BlowChunks where

import qualified Data.Map as M

testInput :: [(Char, Int)]
testInput = [('b',4), ('a',5), ('c',3), ('c',5), ('c',4), 
         ('a',2), ('b',2), ('a',4), ('c',1), ('a',1), 
         ('a',3), ('b',1), ('b',5), ('b',3), ('c',2) ]

testExpected :: [(Char, [Int])] 
testExpected = [('c',[3,5,4]),('a',[5,2,4]),('b',[4,2,1]),('a',[1,3]),('b',[5,3]),('c',[1,2])]

blowChunks :: (Ord a) => Int -> [(a, b)] -> [(a, [b])]
blowChunks n = (fmap . fmap) reverse . go M.empty
  where
    go m [] = M.toList m
    go m ((a, b) : xs) = case M.lookup a m of
      Nothing -> go (M.insert a [b] m) xs
      Just ys
        | length ys < n - 1 -> go (M.insert a (b : ys) m) xs
        | otherwise -> (a, (b : ys)) : go (M.delete a m) xs

blowChunks' :: (Ord a) => Int -> [(a, b)] -> [(a, [b])]
blowChunks' n = (fmap . fmap) reverse . go M.empty
  where
    go m [] = M.toList m
    go m ((a, b) : xs) = case M.alterF f a m of
      (Nothing, m') -> go m' xs
      (Just r, m') -> r : go m' xs
      where
        f Nothing = (Nothing, Just [b])
        f (Just ys) 
          | length ys < n - 1 = (Nothing, Just (b : ys))
          | otherwise         = (Just (a, (b : ys)), Nothing)

test :: Bool
test = blowChunks' 3 testInput == testExpected

testStream :: [(Char, [Int])]
testStream = take 10 $ blowChunks' 3 $ cycle testInput 
