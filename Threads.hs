{-# language TypeSynonymInstances, FlexibleInstances, 
      GADTs, KindSignatures, GeneralizedNewtypeDeriving #-}

module Threads where

newtype Thread a = Thread {runThread :: IO a}
  deriving Monad

data V :: * -> * where
  VString :: String -> V String
  VInt    :: Int    -> V Int

class ToV a where
  v :: a -> V a

instance ToV String where
  v = VString

instance ToV Int where
  v = VInt

out :: V String -> Thread ()
out (VString s) = Thread $ putStrLn s

toString :: V Int -> V String
toString (VInt i) = VString $ show i

dec :: V Int -> V Int
dec (VInt i) = VInt $ i - 1

ifZero :: V Int -> Thread a -> Thread a -> Thread a
ifZero (VInt i) t f = if i == 0 then t else f

------------------------------------------------------------

test :: Thread ()
test = go (v 5)
  where
    go i = ifZero i (return ()) (out (toString i) >> go (dec i))

main :: IO ()
main = runThread test