-- https://www.stackage.org/lts-20.0
-- https://hackage.haskell.org/package/mongoDB-2.7.1.2/docs/Database-MongoDB-Query.html
-- https://treehouse.github.io/installation-guides/mac/mongo-mac.html
-- ./mongod --dbpath ~/data/db

module Main (main) where
{-# LANGUAGE OverloadedStrings, ExtendedDefaultRules #-}
import Database.MongoDB as M
import Data.String (fromString)
main :: IO ()
main = do
  let db = "test"
      collection = "mydetails"
      doc = ["name" =: "John", "age" =: 30]
  pipe <- connect (host "localhost")
  eResult <- access pipe master (fromString db) $ do
    insert collection doc
  close pipe
  case eResult of
    Left failure -> print failure
    Right _ -> putStrLn "Document inserted successfully"

  -- let runMongo action = access pipe master db action
  -- docs <- runMongo $ find (select [] "myCollection")
  -- Prelude.mapM_ print docs
  -- close pipe



