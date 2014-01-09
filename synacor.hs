{-# language RecordWildCards #-}

module Synacor where

import qualified Data.IntMap as M
import Data.Word
import Data.Char

data State = State
  { mem   :: !(M.IntMap Word16)
  , stack :: ![Word16]
  , ip    :: !Int
  }

run :: State -> String -> String
run s@State{..} input = case  mem M.! ip of
  0  -> []
  19 -> chr (fromIntegral (mem M.! (ip+1))) : run s{ip = ip + 2} input -- Need to handle registers too
  21 -> run s{ip = ip + 1} input

load :: [Word16] -> State
load prog = State (M.fromList $ zip [0..] prog) [] 0