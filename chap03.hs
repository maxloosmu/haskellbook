module Chap03 where

-- -- error: Variable not in scope: d
-- area d = pi * (r * r)
-- r = d / 2

area2 d = pi * (r * r)
  where r = d / 2

-- 3.6 Concatenation and scoping
--------------------------------
main :: IO ()
main = do
  putStrLn greeting
  printSecond
  where
    greeting = "Yarrrrr"
    printSecond =
      putStrLn greeting

-- 3.8 Chapter exercises
------------------------
returnA = "Curry is awesome" ++ "!"
returnB = [("Curry is awesome" ++ "!") !! 4]
returnC = drop 9 $ "Curry is awesome" ++ "!"

retA :: String -> String
retA x = x ++ "!"
retB :: String -> Int -> String
retB x y = [(x ++ "!") !! y]
retC :: String -> Int -> String
retC x y = drop y $ x ++ "!"
thirdLetter :: String -> Char
thirdLetter x = x !! 2
letterIndex :: Int -> Char
letterIndex y = "Curry is awesome!" !! y

-- question 5
input = words "Curry is awesome"
input2 = "Curry is awesome"
rvrs :: [String] -> String
rvrs [] = []
rvrs x = unwords $ rvrs (drop 1 x) : take 1 x
rvrs2 = drop 1 $ rvrs input
rvrs3 :: String -> String
rvrs3 x =
  let curry = take 1 (words x)
      is = take 1 $ drop 1 $ words x
      awesome = take 1 $ drop 2 $ words x
  in unwords $ awesome ++ is ++ curry

-- question 6
main2 :: IO ()
main2 = do
  print rvrs2
  print (rvrs3 input2)



