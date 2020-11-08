module Cipher where

import Test.QuickCheck 
import Data.Char

let2int :: Char -> Int
let2int c = ord c - ord 'a'
upplet2int :: Char -> Int
upplet2int c = ord c - ord 'A'
int2let :: Int -> Char
int2upplet :: Int -> Char
int2let n = chr (ord 'a' + n)
int2upplet n = chr (ord 'A' + n)
shift :: Int -> Char -> Char
shift n c 
  | isLower c = int2let ((let2int c + n) `mod` 26)
  | isUpper c = int2upplet ((upplet2int c + n) `mod` 26)
  | otherwise = c
encode :: Int -> [Char] -> [Char]
encode n xs = [shift n x | x <- xs]
decode :: Int -> [Char] -> [Char]
decode n xs = [shift (negate n) x | x <- xs]

genString :: Gen [Char]
genString = elements ["a".."z"]
genNum :: Gen Int
genNum = oneof [0..100]
test :: IO ()
test = sample genNum
-- -- wrong because Gen is not data constructor:
-- pick = go genNum where
--   go (Gen x) = x

-- check :: IO ()
-- check = do
--   x <- sample genNum
--   quickCheck (prop_checkChar x)

prop_checkChar :: Int -> Property
prop_checkChar x = 
  forAll genString
    (\c ->     
      encode x c 
      -- >>= decode x (encode x c) 
      == c)




