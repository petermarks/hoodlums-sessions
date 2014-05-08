{-# language GADTs, KindSignatures #-}

module Threads where

newtype Thread a = Thread {runThread :: IO a}

data V :: * -> * where
  VString :: String -> V String

v :: String -> V String
v = VString

out :: V String -> Thread ()
out (VString s) = Thread $ putStrLn s

------------------------------------------------------------

test :: Thread ()
test = out $ v "Hello"

main :: IO ()
main = runThread test