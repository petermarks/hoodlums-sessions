{-# language OverloadedStrings #-}

module SQL where

import Database.SQLite3
import Data.Text
import Data.Int (Int64)
import Data.Function

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

processRow :: Statement -> IO ()
processRow statement = do
  idx       <- columnInt64 statement 0
  title     <- columnText statement 1
  artistIdx <- columnInt64 statement 2
  print $ Album idx title artistIdx

execQuery :: Database -> Text -> (Statement -> IO ()) -> IO ()
execQuery db q rowProcessor = do
  statement <- prepare db q
  fix $ \loop -> do
    stepResult <- step statement
    case stepResult of
      Row -> do
        rowProcessor statement
        loop
      Done ->
        return ()