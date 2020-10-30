module DogsRule
  ( dogs, twoo, twoo2, twoo3 )
  where

dogs :: IO ()
dogs = do
  putStrLn "Who's a good puppy?!"
  putStrLn "YOU ARE!!!!!"

twoo :: IO Bool
twoo = 
  do 
    c <- getChar
    c' <- getChar
    return (c == c')

twoo2 :: IO ()
twoo2 = 
  do 
    c <- getChar
    c' <- getChar
    if (c == c')
    then putStrLn "\nTrue"
    else return ()

twoo3 :: IO String
twoo3 = 
  do 
    c <- getChar
    c' <- getChar
    if (c == c')
    then return "True"
    else return "False"
