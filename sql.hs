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
  untilM_ testDone (processRow statement)

testDone :: StepResult -> Bool
testDone Done = True
testDone Row  = False

processRow :: Statement -> IO StepResult
processRow statement = do
  stepResult <- step statement
  case stepResult of
    Row -> do
      cols <- columns statement
      print cols
    Done ->
      putStrLn "All done"
  return stepResult

untilM_ :: (a -> Bool) -> IO a -> IO ()
untilM_ p a = do
  r <- a
  if p r then return () else untilM_ p a