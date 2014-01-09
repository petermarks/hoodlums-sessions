module Synacor where

import Control.Applicative
import Control.Monad.State.Strict
import qualified Data.IntMap as M
import Data.Word
import Data.Char

data Machine = Machine
  { mem   :: !(M.IntMap Word16)
  , stack :: ![Word16]
  , ip    :: !Int
  }

type Syn = State Machine

run :: Machine -> String -> String
run m i = evalState (run' i) m

run' :: String -> Syn String 
run' input = getMem 0 >>= \inst -> case inst of
  0  -> return []
  19 -> (:) <$> (chr . fromIntegral <$> getMem 1) <*> (incip 2 >> run' input) -- Need to handle registers too
  21 -> incip 1 >> run' input

incip :: Int -> Syn ()
incip n = modify $ \s -> s{ip = ip s + n}

getMem :: Int -> Syn Word16
getMem offset = (M.!) <$> gets mem <*> gets ((+ offset) . ip)

load :: [Word16] -> Machine
load prog = Machine (M.fromList $ zip [0..] prog) [] 0