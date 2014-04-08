{-# language TypeSynonymInstances, FlexibleInstances #-}

module DamageDice where

import System.Random
import Control.Monad.State
import Control.Applicative


-------------------------------------------------------------
-- Interpreter class

class Interpreter i where
  roll :: Int -> i
  mult :: Int -> i -> i
  add  :: i -> i -> i
  lit  :: Int -> i


-------------------------------------------------------------
-- Roller instance

type Roller = State StdGen

runRoller :: Roller Int -> StdGen -> Int
runRoller = evalState

instance Interpreter (Roller Int) where
  roll x = do
    gen <- get
    let (n, gen') = randomR (1, x) gen
    put gen'
    return n

  mult x r = sum <$> replicateM x r

  add = liftA2 (+)

  lit = pure


-------------------------------------------------------------
-- Range instance

newtype Range = Range (Int, Int)
  deriving (Show)

instance Interpreter Range where
  roll x = Range (1, x)
  mult x (Range (rmin, rmax)) = Range (rmin * x, rmax * x)
  add (Range (lmin, lmax)) (Range (rmin, rmax)) = Range (lmin + rmin, lmax + rmax)
  lit x = Range (x, x)


-------------------------------------------------------------
-- Main

test :: (Interpreter i) => i
test = mult 2 (roll 6 ) `add` lit 17

main :: IO ()
main = do
  gen <- newStdGen
  let n = runRoller test gen
  print n
  print (test :: Range)
