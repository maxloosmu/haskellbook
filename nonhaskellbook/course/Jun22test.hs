{-# OPTIONS_GHC -Wall #-}
module Jun16test where

import Prelude hiding (map, any)

map :: (a -> b) -> [a] -> [b]
map _ [] = []
map f (x : xs) = f x : map f xs

filter1 :: (a -> Bool) -> [a] -> [a]
filter1 _ [] = []
filter1 f (x : xs) = 
  if f x 
    then x : filter1 f xs 
    else filter1 f xs

filter2 :: (a -> Bool) -> [a] -> [a]
filter2 _ [] = []
filter2 f (x : xs) = 
  case f x of
    True -> x : filter2 f xs
    False -> filter2 f xs

filter3 :: (a -> Bool) -> [a] -> [a]
filter3 _ [] = []
filter3 f (x : xs)
  | f x = x : filter3 f xs
  | otherwise = filter3 f xs

any :: (a -> Bool) -> [a] -> Bool
any _ [] = False
any f (x : xs) = f x || any f xs

