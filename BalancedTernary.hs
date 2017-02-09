module BalancedTernary where

import Data.List

data BTDigit = Minus | Zero | Plus deriving (Eq, Ord)

type BTNum = [BTDigit]

instance Enum BTDigit where
  fromEnum Minus = -1
  fromEnum Zero  = 0
  fromEnum Plus  = 1
  toEnum (-1) = Minus
  toEnum 0    = Zero
  toEnum 1    = Plus
  toEnum i    = error $ "toEnum BTDigit of " ++ shows i " out of range"

instance Show BTDigit where
  showsPrec _ Minus = ('-' :)
  showsPrec _ Zero  = ('0' :)
  showsPrec _ Plus  = ('+' :)
  showList = flip (foldr shows)

instance Read BTDigit where
  readsPrec _ [] = []
  readsPrec _ (x : xs) = case x of
    '-' -> [(Minus, xs)]
    '0' -> [(Zero, xs)]
    '+' -> [(Plus, xs)]
    _   -> []
  readList xs = case reads xs of
    []          -> []
    (d, xs) : _ -> case readList xs of
      []           -> [([d], xs)]
      (ds, xs) : _ -> [(d:ds, xs)]

fromInteger' :: Integer -> BTNum
fromInteger' 0 = [Zero]
fromInteger' i = go i []
  where
    go 0 a = a
    go i a = go q $ toEnum (fromInteger (r - 1)) : a
      where
        (q,r) = divMod (i + 1) 3

toInteger' :: BTNum -> Integer
toInteger' = foldl' f 0
  where
    f a d = a * 3 + fromIntegral (fromEnum d)
