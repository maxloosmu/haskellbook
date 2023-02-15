module Main (main) where
{-# LANGUAGE OverloadedStrings, ExtendedDefaultRules #-}
import Database.MongoDB as M
-- https://www.stackage.org/lts-20.0
-- https://hackage.haskell.org/package/mongoDB-2.7.1.2/docs/Database-MongoDB-Query.html
main :: IO ()
main = do
  let db = "test"
  pipe <- connect (host "localhost")
  e <- access pipe master db allCollections
  close pipe
  print e
