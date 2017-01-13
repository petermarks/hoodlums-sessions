{-# language RecordWildCards #-}

module Analysers where

import Data.List
import Control.Monad

data Analyser s = Analyser
  { initial  :: s
  , analyse  :: s -> Char -> s
  , finish   :: s -> String
  }

data Pair a b = Pair !a !b

count :: Analyser Int
count = Analyser
  { initial = 0
  , analyse = \i _ -> (i + 1)
  , finish  = show
  }

freq :: Analyser (Pair Int Int)
freq = Analyser
  { initial = 0 `Pair` 0
  , analyse = \(Pair t f) c -> if c == 'a' then (t + 1) `Pair` f else t `Pair` (f + 1)
  , finish  = \(Pair t f) -> show $ fromIntegral t / fromIntegral (t + f)
  }

pair :: Analyser s1 -> Analyser s2 -> Analyser (Pair s1 s2)
pair a b = Analyser
  { initial = initial a `Pair` initial b
  , analyse = \(Pair sa sb) c -> analyse a sa c `Pair` analyse b sb c
  , finish  = \(Pair sa sb) -> finish a sa ++ '\n' : finish b sb
  }

process :: Analyser s -> String -> String
process Analyser{..} = finish . foldl' analyse initial

data AnalyserA = AnalyserA
  { analyseA  :: Char -> AnalyserA
  , finishA   :: String
  }

countA :: AnalyserA
countA = go 0
  where
    go i = AnalyserA
      { analyseA = \_ -> go $! i + 1
      , finishA  = show i
      }

freqA :: AnalyserA
freqA = go (0 `Pair` 0)
  where
    go (Pair t f) = AnalyserA
      { analyseA = \c -> go $! if c == 'a' then (t + 1) `Pair` f else t `Pair` (f + 1)
      , finishA  = show $ fromIntegral t / fromIntegral (t + f)
      }

mkA :: Analyser s -> AnalyserA
mkA Analyser{..} = go initial
  where
    go s = AnalyserA
      { analyseA = \c -> go $! analyse s c
      , finishA  = finish s
      }

processA :: [AnalyserA] -> String -> String
processA as = unlines . map finishA . foldl' (\as' c -> (flip analyseA c) <$!> as') as

main :: IO ()
main = do
  text <- readFile "book.txt"
  -- putStrLn $ process (pair count freq) text
  -- putStrLn $ processA [countA, freqA] text
  putStrLn $ processA [mkA count, mkA freq] text
