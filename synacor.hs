{-# language RecordWildCards #-}

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
run' input = getMem >>= \inst -> case inst of
  0  -> return []
  19 -> do
    c <- chr . fromIntegral <$> getValue
    (c :) <$> run' input
  20 -> do
    let (c:input') = input
    addr <- getAddr
    store addr (fromIntegral $ ord c)
    run' input'
  21 -> getMem >> run' input

retrieve :: Int -> Syn Word16
retrieve addr = (M.! addr) <$> gets mem

store :: Int -> Word16 -> Syn ()
store addr value = modify $ \s -> s{mem = M.insert addr value (mem s)}

getMem :: Syn Word16
getMem = do
  m@Machine{..} <- get
  put $ m{ip = ip + 1}
  return $ mem M.! ip

getAddr :: Syn Int
getAddr = fromIntegral <$> getMem

getValue :: Syn Word16
getValue = do
  i <- getMem
  if i > 32767 then retrieve (fromIntegral i) else return i

load :: [Word16] -> Machine
load prog = Machine (M.fromList $ zip [0..] prog) [] 0