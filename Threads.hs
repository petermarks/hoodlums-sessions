module Threads where

type Thread = IO

runThread :: Thread a -> IO a
runThread = id

out :: String -> Thread ()
out = putStrLn

test :: Thread ()
test = out "Hello"

main :: IO ()
main = runThread test