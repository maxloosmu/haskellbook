{-  -}
{-  
-- HaskellBook Chap 8 Recursion
-- {-# LANGUAGE FlexibleContexts, NoMonomorphismRestriction #-}
divBy :: Integral a => a -> a -> (a, a)
divBy num denom = go num denom 0 0 where 
  go n d count neg
    | d == 0 = error "Divide by Zero!  "
    | n < 0 && d > 0 = go (negate n) d count 1
    | d < 0 && n > 0 = go n (negate d) count 1
    | n < 0 && d < 0 = go (negate n) (negate d) count 0
    | n < d && neg == 1 = (negate count, n)
    | n < d = (count, n)
    | n >= d =
      go (n - d) d (count + 1) neg
test = divBy2 (-5) (0)
data DivResult =
  Result Integer | 
  DivByZero 
  deriving Show
divBy2 num denom = go num denom 0 0 where 
  go n d count neg
    | d == 0 = (DivByZero, n)
    | n < 0 && d > 0 = go (negate n) d count 1
    | d < 0 && n > 0 = go n (negate d) count 1
    | n < 0 && d < 0 = go (negate n) (negate d) count 0
    | n < d && neg == 1 = (Result (negate count), n)
    | n < d = (Result count, n)
    | n >= d =
      go (n - d) d (count + 1) neg
-- -- this doesn't work, cannot match (-n):
-- -- Parse error in pattern: -n 
-- divBy3 num denom = go num denom 0 where 
--   go n d count
--     | n < d = (count, n)
--     | otherwise =
--       go (n - d) d (count + 1)
--   go (-n) d count
--     | n < d = ((-count), n)
--     | n >= d =
--       go (n - d) d (count + 1)
-- -- Equational Reasoning:
-- divBy 5 2
-- = go 5 2 0
-- = go 3 2 1
-- = go 1 2 2 
-- = (2, 1)
sumcount :: (Eq a, Num a) => a -> a
sumcount n
  | n == 1 = 1
  | otherwise = n + sumcount (n - 1)
-- x becomes the count, y becomes incremental value
multBy :: (Eq t1, Num t1, Num t2) => t1 -> t2 -> t2
multBy x y = go x y 0 where
  go x y sum
    | x == 0 = sum
    | otherwise = go (x - 1) y (sum + y)
-}


import Data.List (intersperse)
mc91 n 
  | n > 100 = n - 10
  | otherwise = mc91 (mc91 (n + 11))
test = map mc91 [95..110]
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
digits :: Int -> [Int]
digits n 
  | n == 0 = []
  | otherwise = (digits $ n `div` 10) ++ 
    [n `mod` 10]
test2 = wordNum 108
-- once $ is used, point free not possible
wordNum :: Int -> String
wordNum = concat . intersperse "-" . 
  map digit2Word . digits 
wordNum2 n = concat $ intersperse "-" $ 
  map digit2Word $ digits $ n


