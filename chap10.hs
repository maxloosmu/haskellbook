{-  -}
{-  
-- Haskell Programming from first principles
-- Chapter 10 - Folding Lists, 
-- bits from Chapter 8 - Recursion
import Data.Time
data DatabaseItem = DbString String
  | DbNumber Integer
  | DbDate UTCTime
  deriving (Eq, Ord, Show)
theDatabase :: [DatabaseItem]
theDatabase =
  [ DbDate (UTCTime
    (fromGregorian 1911 5 1)
    (secondsToDiffTime 34123))
  , DbNumber 9001
  , DbNumber 9009
  , DbString "Hello, world!"
  , DbDate (UTCTime
    (fromGregorian 1921 5 1)
    (secondsToDiffTime 34123))
  ]
filterNum = filter (\x -> x == 
  DbNumber 9001) theDatabase
filterDate = map getUTC (filter getDate theDatabase) where
  getDate (DbDate _) = True
  getDate (_) = False
  getUTC (DbDate utc) = utc
datetime = foldr getUTC [] theDatabase where
  getUTC (DbDate x) acc = x : acc
  getUTC _ acc = acc
filterNumber = foldr getNum [] theDatabase where
  getNum (DbNumber x) acc = x : acc
  getNum _ acc = acc
recent = maximum datetime
average = fromIntegral (sum filterNumber) 
  / fromIntegral (length filterNumber)
-- Original wrong equations:  
-- datetime = foldr getUTC [] theDatabase where
--   getUTC (DbDate x : xs) = x : xs
--   getUTC (_ : xs) = xs
-- New equations, Equational reasoning start: 
-- datetime = foldr getUTC [] [DbDate 1, DbNumber 9001] where
  -- getUTC (DbDate x) = x 
  -- getUTC _ = []
-- Step one: 
-- datetime = getUTC (DbDate 1) 
--   (foldr getUTC [] [DbNumber 9001])
-- Step two: 
-- datetime = 1 getUTC (DbNumber 9001) 
--   (foldr getUTC [] [])
-- FAIL: how to link 1 to next in sequence
-- New equations, Equational reasoning start: 
-- datetime = foldr getUTC [] [DbDate 1, DbNumber 9001] where
--   getUTC (DbDate x) acc = x : acc
--   getUTC _ acc = acc
-- -- Step one (acc is [], follow type signature): 
-- datetime = getUTC (DbDate 1) acc
--   (foldr getUTC [] [DbNumber 9001])
-}

{-  
fibs = 1 : scanl (+) 1 fibs
fibs20 = take 20 fibs
fibs100 = [x | x <- fibs20, x < 100]
fibs200 = takeWhile (< 200) fibs20
incTimes :: (Num a, Num b) => b -> a -> a
incTimes 0 n = n
incTimes times n = 1 + (incTimes (times - 1) n)

factorial 0 = 1
factorial n = n * factorial (n - 1)
factorial2 n = scanl (*) 1 [1..n] !! n

-- -- For scanl:
-- scanl2 :: (a -> b -> a) -> a -> [b] -> [a]
-- scanl2 f q ls =
--   q : (case ls of
--     [] -> []
--     x:xs -> scanl2 f (f q x) xs)
-- scanl (+) 1 [1,2]
-- = 1 : (scanl (+) ((+) 1 1) [2])
-- = 1 : (scanl (+) 2 [2])
-- = 1 : 2 : (scanl (+) ((+) 2 2) [])
-- = 1 : 2 : (scanl (+) 4 [])
-- = 1 : 2 : 4 : []
-- -- For fibs: 
-- fibs = 1 : scanl (+) 1 fibs
-- -- read 1st fibs = 1:
-- = 1 : 1 : (scanl (+) ((+) 1 1) fibs) 
-- = 1 : 1 : scanl (+) 2 fibs
-- -- read 2nd fibs = 1:
-- = 1 : 1 : 2 : (scanl (+) ((+) 2 1) fibs) 
-- = 1 : 1 : 2 : scanl (+) 3 fibs
-- -- read 3rd fibs = 2:
-- = 1 : 1 : 2 : 3 : (scanl (+) ((+) 3 2) fibs) 
-- = 1 : 1 : 2 : 3 : scanl (+) 5 fibs
-- = etc
-}

{-  
svs1 = [[x, y, z] | 
  x <- "pbtdkg", y <- "aeiou", z <- "pbtdkg"]
-- -- svs2 fails, cannot use foldr for this task
-- svs2 = foldr vowel [] "pbtdkg" where
--   vowel = foldr stop [] "aeiou" where
--     stop = [x | x <- "pbtdkg"]
svs3 = [['p', y, z] | 
  y <- "aeiou", z <- "pbtdkg"]
nouns = ["pad", "pet", "pit", "pig", "pot", "pod", "pub", "pup"]
verbs = ["pat", "pip", "pit", "pop"]
nvn1 = [(x, y, z) | x <- nouns, y <- verbs, z <- nouns]
-}

{-  
check = myOr8 [False, False, True]
myOr1 [] = False
myOr1 (x:xs) = x || myOr1 xs
myOr2 [] = False
myOr2 (x:xs) = case x of
  True -> True
  False -> myOr2 xs
myOr3 [] = False
myOr3 (x:xs) 
  | x == True = True
  | otherwise = myOr3 xs
myOr4 [] = False
myOr4 (x:xs) = 
  if x == True
  then True
  else myOr4 xs
-- ghci throws error on myOr5, myOr6, myOr7
-- if no type annotation/signature, hugs no error
myOr5 :: [Bool] -> Bool
myOr5 = foldr
  (\a b -> case a of
    True -> True
    False -> b) False
myOr6 :: [Bool] -> Bool
myOr6 = foldr (||) False
myOr7 :: [Bool] -> Bool
myOr7 = foldl (||) False
myOr8 = foldl
  (\b a -> case a of
    True -> True
    False -> b) False
myOr9 = foldr
  (\a b -> if a == 1
    then 1
    else b) 0

check2 = myAny odd [2, 4, 6]
myAny f = myOr8 . map f
check3 = myElem 1 [1..5]
myElem x = myOr8 . 
  foldr (\a b -> (a == x):b) [] 
myElem2 x ys = myOr8 $ 
  foldr (\a b -> (a == x):b) [] ys
-- -- Equational Reasoning for myElem's foldr, 
-- -- x = 1, ys = [1]:
-- test = foldr (\a b -> (a == 1):b) [] [1]
-- test2 = (\a b -> (a == 1):b) 1 
--   (foldr (\a b -> (a == 1):b) [] [])
-- test3 = (\a b -> (a == 1):b) 1 []

check4 = myElem3 1 [1..]
-- myElem3 can be used to find existence of
-- element in infinite list, myElem & myElem2
-- will throw error: C stack overflow.  
-- Element must exist, else program cannot stop.
myElem3 x = foldr (\a b -> a == x || b) False
myElem4 x = any (x==)
-}

{-  
myRev [] = []
myRev (x:xs) = myRev xs ++ [x]
check = (:) 1 [2]
myRev2 = foldl (flip (:)) [] 
test = myRev4 "asdf"
myRev3 = foldr (\a b -> b ++ [a]) []
myRev4 = foldl (\b a -> a : b) []

myMap f [] = []
myMap f (x:xs) = (f x) : myMap f xs
myMap2 f = foldr (\a b -> (f a) : b) []
-- myMap3 requires myRev2 because foldl 
-- auto reverses output list, see myRev4
myMap3 f = myRev2 . 
  foldl (\b a -> (f a) : b) []
test2 = myMap3 (*2) [1..3]

myFilter f [] = []
myFilter f (x:xs) 
  | f x == True = x : myFilter f xs
  | otherwise = myFilter f xs
myFilter2 f = foldr 
  (\a b -> 
    if (f a == True) 
    then a : b
    else b) []
test3 = myFilter2 odd [1..4]

squish [] = []
squish (x:xs) = x ++ squish xs
test4 = squish2 ["ads", "jkl"]
squish2 = foldr (\a b -> a ++ b) []

sqMap f [] = []
sqMap f (x:xs) = f x ++ sqMap f xs
f x = "WO " ++ [x] ++ " OT "
test5 = sqMap2 f "blah"
sqMap2 f = foldr (\a b -> (f a) ++ b) []
test6 = sqAgain ["ads", "jkl"]
sqAgain = sqMap (\x -> x ++ [])

myMaxBy f [x] = x
myMaxBy f (x:xs) = case f x myMax of
  LT -> myMax
  EQ -> x
  GT -> x
  where myMax = myMaxBy f xs
test7 = myMaxBy2 compare [1..10]
test8 = myMaxBy (\_ _ -> GT) [1..10]
myMaxBy2 f = foldr1
  (\a b ->
    if f a b == LT
    then b
    else a)

myMinBy f [x] = x
myMinBy f (x:xs) = case f x myMin of
  GT -> myMin
  EQ -> x
  LT -> x
  where myMin = myMinBy f xs
test9 = myMinBy compare [1..10]
-}


