{-# language OverloadedStrings #-}

module SQL where

import Database.SQLite3
import Data.Text

dbname = "chinook.sqlite" -- Change this to your DB
query  = "select * from album;" -- Whatever query you like

main :: IO ()
main = do
  db <- open dbname
  statement <- prepare db query
  processRows statement

processRows :: Statement -> IO ()
processRows statement = do
  stepResult <- step statement
  case stepResult of
    Row -> do
      cols <- columns statement
      print cols
      processRows statement
    Done ->
      putStrLn "All done"