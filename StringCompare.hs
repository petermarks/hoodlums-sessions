{-# language ViewPatterns #-}
module StringCompare where

import Prelude hiding (compare)

data Symbol = U | L | BS | CL deriving (Eq, Show)

-- WE NEED LISTS GIVEN TO US REVERSED!!!

compare :: [Symbol] -> [Symbol] -> Bool
compare (next 0 False -> (bx, xs)) (next 0 False -> (by, ys)) = case (xs, ys) of
  ([], []) -> True
  ([], _)  -> False
  (_,  []) -> False
  (x : xs, y : ys) -> x == y && compare xs ys 

next :: Int -> Bool -> [Symbol] -> (Bool, [Symbol])
next _ b [] = (b, [])
next n b (BS : xs) = next (n + 1) b xs
next n b (CL : xs) = next n (not b) xs
next 0 b xs = (b, xs)
next n b (_ : xs) = next (n - 1) b xs

s1 = reverse [BS, U, L, U]
s2 = reverse [U, L, L, BS, U]

