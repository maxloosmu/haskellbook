module WordNumber where

import Data.List (intersperse)

digit2Word :: Int -> String
digit2Word n = case n of
  0 -> "zero"
  1 -> "one"
  2 -> "two"
  3 -> "three"
  4 -> "four"
  5 -> "five"
  6 -> "six"
  7 -> "seven"
  8 -> "eight"
  9 -> "nine"
  _ -> "ERR"
digits :: Int -> [Int]
digits n 
  | n == 0 = []
  | otherwise = (digits $ n `div` 10) ++ 
    [n `mod` 10]
test2 :: String
test2 = wordNum 108
-- once $ is used, point free not possible
wordNum :: Int -> String
wordNum = concat . intersperse "-" . 
  map digit2Word . digits 
wordNum2 :: Int -> String
wordNum2 n = concat $ intersperse "-" $ 
  map digit2Word $ digits $ n
