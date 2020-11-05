module Main where

import Test.QuickCheck
import Data.List (sort)
import Data.Char

main :: IO ()
main = putStrLn "hello!"

half :: Fractional a => a -> a
half x = x / 2
halfIdentity :: Double -> Double
halfIdentity = (* 2) . half
prop_halfIdentity :: Double -> Bool
prop_halfIdentity x = 
  halfIdentity x == x

runqc :: IO ()
runqc = quickCheck prop_reverse

prop_listOrdered :: [Char] -> Bool
prop_listOrdered xs = listOrdered $ sort xs
-- for any list you apply sort to,
-- this property should hold
listOrdered :: (Ord a) => [a] -> Bool
listOrdered xs =
  snd $ foldr go (Nothing, True) xs where 
    go _ status@(_, False) = status
    go y (Nothing, t) = (Just y, t)
    go y (Just x, _) = (Just y, x >= y)
-- -- Equational Reasoning without go extensions
-- test1 = listOrdered [1,2]
-- test2 = snd $ go 1 (foldr go (Nothing, True) [2])
-- test3 = 
--   snd $ go 1 (go 2 (foldr go (Nothing, True) []))
-- test4 = snd $ go 1 (go 2 (Nothing, True))
-- test5 = snd $ go 1 (Just 2, True)
-- test6 = snd $ (Just 1, True)

prop_plus :: Int -> Int -> Int -> Bool
prop_plus x y z = 
  (plusAssociative x y z) && 
  (plusCommutative x y) &&
  (plusCommutative x z) &&
  (plusCommutative y z)
plusAssociative :: (Eq a, Num a) => a -> a -> a -> Bool
plusAssociative x y z =
  x + (y + z) == (x + y) + z
plusCommutative :: (Eq a, Num a) => a -> a -> Bool
plusCommutative x y =
  x + y == y + x

prop_mult :: Int -> Int -> Int -> Bool
prop_mult x y z = 
  (multAssociative x y z) && 
  (multCommutative x y) &&
  (multCommutative x z) &&
  (multCommutative y z)
multAssociative :: (Eq a, Num a) => a -> a -> a -> Bool
multAssociative x y z =
  x * (y * z) == (x * y) * z
multCommutative :: (Eq a, Num a) => a -> a -> Bool
multCommutative x y =
  x * y == y * x

prop_quotdiv :: Int -> Int -> Bool
prop_quotdiv x y = 
  (quotrem x y == x) && (divmod x y == x)
-- quotrem x 0 = x and divmod x 0 = x 
-- is just to avoid divide by zero error
quotrem :: Integral a => a -> a -> a
quotrem x 0 = x
quotrem x y = (quot x y) * y + (rem x y)
divmod :: Integral a => a -> a -> a
divmod x 0 = x
divmod x y = (div x y) * y + (mod x y)

prop_expcomm :: 
  Int -> Int -> Int -> Bool
prop_expcomm x y z = 
  (expCommutative x y) &&
  (expCommutative x z) &&
  (expCommutative y z)
prop_expAssociative :: 
  Int -> Int -> Int -> Bool
prop_expAssociative x y z =
  x ^ (y ^ z) == (x ^ y) ^ z
expCommutative :: Int -> Int -> Bool
expCommutative x y =
  x ^ y == y ^ x

runqc2 :: IO ()
runqc2 = do
  quickCheck prop_capword
  quickCheck prop_sort

prop_reverse ::  String -> Bool
prop_reverse xs = 
  (reverse . reverse) xs == id xs

prop_app :: Fun String Int -> String -> Bool
prop_app (Fn f) s = f s == (f $ s)
prop_comp :: 
  Fun Int String -> Fun String Int -> 
  String -> Bool
prop_comp (Fn f) (Fn g) x = 
  (f . g) x == f (g x)

prop_foldcons :: [Char] -> [Char] -> Bool
prop_foldcons x y = foldr (:) x y == (++) y x
prop_foldconcat :: [String] -> Bool
prop_foldconcat xs = 
  foldr (++) [] xs == concat xs

-- prop_length set to True to avoid
-- n < 0 and xs = [] and length xs < n
prop_length :: Int -> [Char] -> Bool
prop_length _ [] = True
prop_length n xs 
  | n >= 0 && (length xs >= n) = 
    length (take n xs) == n
  | otherwise = True
prop_read :: [Int] -> Bool
prop_read x = read (show x) == x

-- prop_squareid will usually fail
-- due to inaccuracy of floating point
-- calculations
prop_square :: Float -> Bool
prop_square x = square x == x * x
square :: Float -> Float
square x = x * x
prop_squareid :: Float -> Bool
prop_squareid x = 
  squareIdentity x == (square . sqrt) x
squareIdentity :: Float -> Float
squareIdentity = square . sqrt

prop_capword :: String -> Bool
prop_capword x = 
  (capitalizeWord x
  == twice capitalizeWord x) &&
  (capitalizeWord x
  == fourTimes capitalizeWord x)
prop_sort :: String -> Bool
prop_sort x = 
  (sort x == twice sort x) &&
  (sort x == fourTimes sort x)
twice :: (String -> String) -> 
  String -> String
twice f = f . f
fourTimes :: (String -> String) -> 
  String -> String
fourTimes = twice . twice
capitalizeWord :: String -> String
capitalizeWord [] = []
capitalizeWord (x:xs) = toUpper x : xs


