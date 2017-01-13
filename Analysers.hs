{-# language BangPatterns, RecordWildCards, ExistentialQuantification #-}

module Analysers where

import Data.List
import Control.Monad

-- Create a pair forcing both values
(*!) :: a -> b -> (a, b)
(*!) !a !b = (a, b)

infixl 1 *!


-- Solution: Compound analyser

data Analyser s = Analyser
  { initial  :: s
  , analyse  :: s -> Char -> s
  , finish   :: s -> String
  }

count :: Analyser Int
count = Analyser
  { initial = 0
  , analyse = \i _ -> (i + 1)
  , finish  = show
  }

freq :: Analyser (Int, Int)
freq = Analyser
  { initial = 0 *! 0
  , analyse = \(t, f) c -> if c == 'a' then (t + 1) *! f else t *! (f + 1)
  , finish  = \(t, f) -> show $ fromIntegral t / fromIntegral (t + f)
  }

pair :: Analyser s1 -> Analyser s2 -> Analyser (s1, s2)
pair a b = Analyser
  { initial = initial a *! initial b
  , analyse = \(sa, sb) c -> analyse a sa c *! analyse b sb c
  , finish  = \(sa, sb) -> finish a sa ++ '\n' : finish b sb
  }

process :: Analyser s -> String -> String
process Analyser{..} = finish . foldl' analyse initial


-- Solution: Automata

data AnalyserA = AnalyserA
  { analyseA  :: Char -> AnalyserA  -- No strictness necessary as function is call is forced on each step
  , finishA   :: String
  }

countA :: AnalyserA
countA = go 0
  where
    go !i = AnalyserA
      { analyseA = \_ -> go $ i + 1
      , finishA  = show i
      }

freqA :: AnalyserA
freqA = go (0, 0)
  where
    go (!t, !f) = AnalyserA
      { analyseA = \c -> go $ if c == 'a' then (t + 1, f) else (t, f + 1)
      , finishA  = show $ fromIntegral t / fromIntegral (t + f)
      }


-- Solution: Automaton wrapper

mkA :: Analyser s -> AnalyserA
mkA Analyser{..} = go initial
  where
    go !s = AnalyserA  -- Strictness necessary to force state
      { analyseA = \c -> go $ analyse s c  -- Strictness here doesn't help as nothing would be forced until the final lambda is called
      , finishA  = finish s
      }

processA :: [AnalyserA] -> String -> String
processA as = unlines . map finishA . foldl' (\as' c -> mapStrict (flip analyseA c) as') as

mapStrict :: (a -> b) -> [a] -> [b]
mapStrict _ [] = []
mapStrict f (x:xs) = ((:) $! f x) $! mapStrict f xs


-- Solution: Existentials and an indexed pair

-- Some general machinary, probably available in various libraries
data IPair f g = forall i . f i :*: g i
newtype I i = I i
data Some f = forall i . Some (f i)

-- Keeps the states paired with the analysers so that types are know to match
processI :: [Some Analyser] -> String -> String
processI as = unlines . finishes . foldl' analyses initials
  where
    initials :: [IPair I Analyser]  -- A list of pairs of matching states and analysers
    initials       = map (\(Some a) -> I (initial a) :*: a) as
    analyses as' c = mapStrict (\(I s :*: a) -> let !s' = I (analyse a s c) in s' :*: a) as'
    finishes       = map (\(I s :*: a) -> finish a s)


-- Top level

main :: IO ()
main = do
  text <- readFile "book.txt"
  -- putStrLn $ process (pair count freq) text
  -- putStrLn $ processA [countA, freqA] text
  -- putStrLn $ processA [mkA count, mkA freq] text
  putStrLn $ processI [Some count, Some freq] text
