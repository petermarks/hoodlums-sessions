{-# language TypeSynonymInstances, FlexibleInstances #-}

module DamageDice where

import System.Random
import Control.Monad.State
import Control.Applicative
import qualified Data.Map as M
import Data.List


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

data Range a = Range a a
  deriving (Show)

instance Functor Range where
  fmap f (Range a b) = Range (f a) (f b)

instance Applicative Range where
  pure a = Range a a
  (Range fa fb) <*> (Range a b) = Range (fa a) (fb b)

instance Interpreter (Range Int) where
  roll x = Range 1 x
  mult x = fmap (* x)
  add    = liftA2 (+)
  lit    = pure


-------------------------------------------------------------
-- Distribution instance

instance Interpreter (M.Map Int Double) where
  roll x   = M.fromList $ zip [1..x] (repeat $ 1 / fromIntegral x)
  mult x   = foldl1' add . replicate x
  add  l r = M.fromListWith (+) [(lk + rk, lv * rv) | (lk, lv) <- M.toList l, (rk, rv) <- M.toList r]
  lit  x   = M.singleton x 1


-------------------------------------------------------------
-- Main

test :: (Interpreter i) => i
test = mult 2 (roll 6 ) `add` lit 17

main :: IO ()
main = do
  gen <- newStdGen
  let n = runRoller test gen
  print n
  print (test :: Range Int)
  print (test :: M.Map Int Double)
