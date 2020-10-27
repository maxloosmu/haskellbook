module Main where

import qualified DogsRule as DR
-- import Hello
import System.IO


main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  hSetBuffering stdin NoBuffering
  -- sayHello
  -- putStr "Please input your name: "
  -- name <- getLine
  -- sayHello2 name
  -- DR.dogs
  putStr 
    "Please input 2 characters and press enter: "
  check <- DR.twoo
  putStrLn (" ")
  putStrLn (show check)
  putStr 
    "Please input 2 characters and press enter: "
  check2 <- DR.twoo2
  putStrLn (" ")
  putStrLn ""
  print check2
  putStr 
    "Please input 2 characters and press enter: "
  check3 <- DR.twoo3
  putStrLn ""
  print check3

