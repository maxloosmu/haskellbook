module Hello 
  (sayHello, sayHello2)
  where

sayHello :: IO ()
sayHello = do
  putStrLn "hello world !!"

sayHello2 :: String -> IO ()
sayHello2 name =
  putStrLn ("Hi " ++ name ++ "!")