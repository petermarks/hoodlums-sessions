module Threads where

newtype Thread a = Thread {runThread :: IO a}

out :: String -> Thread ()
out = Thread . putStrLn

test :: Thread ()
test = out "Hello"

main :: IO ()
main = runThread test