{-# language OverloadedStrings #-}
{-# language FlexibleInstances #-}

module SQL where

import Database.SQLite3
import Data.Text
import Data.Int (Int64)
import Data.Function

class HasHandler h where
  getHandler :: h -> ColumnIndex -> Statement -> IO ()

instance HasHandler (IO ()) where
  getHandler h _ _ = h

instance (HasHandler a) => HasHandler (Int64 -> a) where
  getHandler f i s = do
    v <- columnInt64 s i
    getHandler (f v) (i + 1) s

instance (HasHandler a) => HasHandler (Text -> a) where
  getHandler f i s = do
    v <- columnText s i
    getHandler (f v) (i + 1) s

data Album = Album {
  albumIdx       :: Int64,
  albumTitle     :: Text,
  albumArtistIdx :: Int64
} deriving Show

dbname = "chinook.sqlite" -- Change this to your DB
query  = "select * from album;" -- Whatever query you like

main :: IO ()
main = do
  db <- open dbname
  execQuery db query processRow

processRow :: Int64 -> Text -> Int64 -> IO ()
processRow idx title artistIdx =
  print $ Album idx title artistIdx

execQuery :: (HasHandler a) => Database -> Text -> a -> IO ()
execQuery db q rowProcessor = do
  statement <- prepare db q
  fix $ \loop -> do
    stepResult <- step statement
    case stepResult of
      Row -> do
        getHandler rowProcessor 0 statement
        loop
      Done ->
        return ()