{-# language TypeSynonymInstances, FlexibleInstances, 
      GADTs, KindSignatures #-}

module Threads where

newtype Thread a = Thread {runThread :: IO a}

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

------------------------------------------------------------

test :: Thread ()
test = out $ toString $ v 123

main :: IO ()
main = runThread test