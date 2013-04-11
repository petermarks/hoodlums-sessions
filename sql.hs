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
  stepResult <- step statement
  cols <- columns statement
  print cols