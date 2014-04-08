module DamageDice where

import System.Random
import Control.Monad.State
import Control.Applicative

type Roller = State StdGen

runRoller :: Roller a -> StdGen -> a
runRoller = evalState

roll :: Int -> Roller Int
roll x = do
  gen <- get
  let (n, gen') = randomR (1, x) gen
  put gen'
  return n

mult :: Int -> Roller Int -> Roller Int
mult x r = sum <$> replicateM x r

add :: Roller Int -> Roller Int -> Roller Int
add = liftA2 (+)

lit :: Int -> Roller Int
lit = pure

test :: Roller Int
test = mult 2 (roll 6 ) `add` lit 17

main :: IO ()
main = do
  gen <- newStdGen
  let n = runRoller test gen
  print n
