module DamageDice where

import System.Random

type Roller = IO

roll :: Int -> Roller Int
roll x = randomRIO (1, x)

mult :: Int -> Roller Int -> Roller Int
mult 0 _ = return 0
mult x r = do
  n <- r
  m <- mult (x - 1) r
  return $ n + m

test :: Roller Int
test = mult 2 $ roll 6
