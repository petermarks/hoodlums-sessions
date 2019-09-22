{-# language BangPatterns #-}

module LazyByteStringIO where

import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.ByteString.Lazy.Internal as L
import qualified Data.ByteString.Char8 as S
import qualified Data.ByteString.Internal as S
import System.Mem
import System.Mem.Weak
import Data.Maybe
-- import Foreign.ForeignPtr
-- import Foreign

consume :: [L.ByteString] -> IO ()
consume [] = putStrLn "Done!!!"
consume (l : ls) = do
  L.putStrLn l
  performGC
  consume ls

-- foreign import ccall "wrapper" makeFinalizer :: (Ptr a -> IO ()) -> IO (FunPtr (Ptr a -> IO ()))

main :: IO ()
main = do
  contents <- L.getContents
  case contents of
   L.Chunk _ !(L.Chunk !(S.PS !fp _ _) _) -> do
      cp <- mkWeakPtr fp Nothing
      let ls = L.lines contents
      consume ls
      cp' <- deRefWeak cp
      print (isJust cp')
      L.putStrLn (head ls)