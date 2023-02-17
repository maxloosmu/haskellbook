import Database.MongoDB
main = do
  pipe <- connect (host "localhost")
  let db = "mydb"
      collection = "mycollection"
    -- find how to access collection from mongodb using haskell.
  access pipe master db $ do
    insert collection ["name" =: "John", "age" =: 30]
    docs <- find (select [] collection)
    liftIO $ print docs
  close pipe


