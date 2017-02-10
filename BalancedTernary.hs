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
  showList [] = ('0' :)
  showList ds = flip (foldr shows) ds

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

add :: BTNum -> BTNum -> BTNum
add a b = zeroStrip $ uncurry (:) $ mapAccumR addDigit Zero $ zip a' b'
  where
    a' = zeroPad (lb - la) a
    b' = zeroPad (la - lb) b
    la = length a
    lb = length b

zeroPad :: Int -> BTNum -> BTNum
zeroPad i ds
  | i <= 0    = ds
  | otherwise = Zero : zeroPad (i - 1) ds

zeroStrip :: BTNum -> BTNum
zeroStrip = dropWhile (== Zero)

addDigit :: BTDigit -> (BTDigit, BTDigit) -> (BTDigit, BTDigit)
addDigit c (a, b) = (toEnum c', toEnum (r - 1))
  where
    (c', r) = divMod (fromEnum a + fromEnum b + fromEnum c + 1) 3
