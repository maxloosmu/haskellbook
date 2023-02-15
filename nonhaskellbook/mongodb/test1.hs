import Database.MongoDB
main = do
  pipe <- connect (host "localhost")
  let db = "mydb"
      collection = "mycollection"
  access pipe master db $ do
    insert collection ["name" =: "John", "age" =: 30]
  access pipe master db $ do
    docs <- find (select [] collection)
    liftIO $ print docs



