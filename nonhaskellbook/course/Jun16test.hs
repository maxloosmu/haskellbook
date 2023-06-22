{-# OPTIONS_GHC -Wall #-}
module Jun16test where

import Prelude hiding (length, sum)

length :: [a] -> Int
length [] = 0
length (_ : xs) = 1 + length xs

sum :: [Int] -> Int
sum [] = 0
sum (x : xs) = x + sum xs

sum' :: [Int] -> Int
sum' = foldl (+) 0

sum'' :: [Int] -> Int
sum'' = foldr (+) 0

sum''' :: [Int] -> Int
sum''' xs = _
-- >>> 2 + 5 + sum [1,2,3]
-- 13

-- q: how does foldr works? 

-- a:
-- foldr f z [x1, x2, ..., xn] == f x1 (f x2 ... (f xn z)...)

-- q: how does foldl works? 

-- a:
-- foldl f z [x1, x2, ..., xn] == f (f ... (f (xn z) x1)...) x2
