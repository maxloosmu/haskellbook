{-# LANGUAGE FlexibleContexts #-}

import Data.Char ( toUpper )
import qualified Data.Map as DM
import Data.List (elemIndex)
import Data.Monoid ( Sum(Sum) )
import Control.Applicative ( liftA3 )


--17.5 Applicative in use
-------------------------
l :: Maybe String
l = lookup (3::Integer) [(3, "hello")]
l1 :: Maybe Int
l1 = fmap length l
c :: String -> String
c [] = []
c (x:xs) = toUpper x:xs
l2 :: Maybe String
l2 = c <$> l
m :: DM.Map Integer String
m = DM.fromList [(3, "hello")]
m1 :: Maybe String
m1 = fmap c $ DM.lookup 3 m

--Exercises: Lookups
--------------------
added :: Maybe Integer
added = fmap (+3) (lookup (3::Integer) $ 
  zip [1, 2, 3] [4, 5, 6])

y :: Maybe Integer
y = lookup (3:: Integer) $ zip 
  [1, 2, 3] [4, 5, 6]
z :: Maybe Integer
z = lookup (2:: Integer) $ zip 
  [1, 2, 3] [4, 5, 6]
tupled :: Maybe (Integer, Integer)
tupled = (,) <$> y <*> z

x1 :: Maybe Int
x1 = elemIndex (3:: Integer) 
  [1, 2, 3, 4, 5]
y1 :: Maybe Int
y1 = elemIndex (4:: Integer) 
  [1, 2, 3, 4, 5]
max' :: Int -> Int -> Int
max' = max
maxed :: Maybe Int
maxed = max' <$> x1 <*> y1

xs1 :: [Integer]
xs1 = [1, 2, 3]
ys :: [Integer]
ys = [4, 5, 6]
x2 :: Maybe Integer
x2 = lookup (3::Integer) $ zip xs1 ys
y2 :: Maybe Integer
y2 = lookup (2::Integer) $ zip xs1 ys
summed :: Maybe Integer
summed = sum <$> ((,) <$> x2 <*> y2)

-- Exercise: Identity instance
------------------------------
newtype Identity a = Identity a
  deriving (Eq, Ord, Show)
instance Functor Identity where
  fmap f (Identity a) = Identity (f a)
instance Applicative Identity where
  pure = Identity
  Identity f <*> Identity a = Identity (f a)
type Id = Identity
xs' :: [Integer]
xs' = [9, 9, 9]
xs2 :: [Integer]
xs2 = const <$> xs1 <*> xs'
-- xs2 output: [1,1,1,2,2,2,3,3,3]
mkId :: a -> Identity a
mkId = Identity
xs3 :: Identity [Integer]
xs3 = const <$> mkId xs1 <*> mkId xs'
-- xs3 output: Identity [1,2,3]
-- Identity is lifted, const keeps
-- [1,2,3] and drops [9,9,9]

-- Exercise: Constant instance
------------------------------
newtype Constant a b =
  Constant { getConstant :: a }
  deriving (Eq, Ord, Show)
instance Functor (Constant a) where
  fmap _ (Constant a) = Constant a
instance Monoid a
  => Applicative (Constant a) where
  pure _ = Constant mempty
  (Constant a) <*> (Constant b) = 
    Constant (a <> b)
type C = Constant
f1 :: Constant (Sum Integer) b
f1 = Constant (Sum 1)
g1 :: Constant (Sum Integer) b
g1 = Constant (Sum 2)
test :: Constant (Sum Integer) b
test = f1 <*> g1
-- test output: Constant {getConstant = 
--   Sum {getSum = 3}}
test1 :: Constant (Sum Integer) b
test1 = Constant undefined <*> g1
-- test1 throws error due to undefined
test2 :: Constant String Int
test2 = pure 1 :: Constant String Int
-- test2 output: Constant {getConstant = ""}
-- if only pure 1 without ::, output is 1
-- with warning: Defaulting the following 
-- constraints to type `Integer' (Num a0)
-- arising from the literal `1' 

-- Maybe Applicative
--------------------
validateLength :: Int
  -> String -> Maybe String
validateLength maxLen s =
  if length s > maxLen
  then Nothing
  else Just s
newtype Name =
  Name String deriving (Eq, Show)
newtype Address =
  Address String deriving (Eq, Show)
mkName :: String -> Maybe Name
mkName s =
  Name <$> validateLength 25 s
mkAddress :: String -> Maybe Address
mkAddress a =
  Address <$> validateLength 100 a
data Person =
  Person Name Address
  deriving (Eq, Show)
mkPerson :: String
  -> String -> Maybe Person
mkPerson n a =
  case mkName n of
    Nothing -> Nothing
    Just n' ->
      case mkAddress a of
        Nothing -> Nothing
        Just a' ->
          Just $ Person n' a'
s1 :: String 
s1 = "old macdonald's"
addy :: Maybe Address
addy = mkAddress s1
b1 :: Maybe Name
b1 = mkName "Babe"
person :: Maybe (Address -> Person)
person = Person <$> b1
test3 :: Maybe Person
test3 = Person <$> b1 <*> addy
-- test3 output: Just (Person (Name "Babe") 
--   (Address "old macdonald's"))
-- cannot do: fmap (fmap Person b1) addy
mkPerson1 :: String
  -> String -> Maybe Person
mkPerson1 n a =
  Person <$> mkName n <*> mkAddress a
test4 :: Maybe Person
test4 = mkPerson1 "Babe" "old macdonald's"
-- test4 output: Just (Person (Name "Babe") 
--   (Address "old macdonald's"))

data Cow = Cow {
  name :: String
, age :: Int
, weight :: Int
  } deriving (Eq, Show)
noEmpty :: String -> Maybe String
noEmpty "" = Nothing
noEmpty str = Just str
noNegative :: Int -> Maybe Int
noNegative n 
  | n >= 0 = Just n
  | otherwise = Nothing
cowFromString :: String
  -> Int -> Int -> Maybe Cow
cowFromString name' age' weight' =
  case noEmpty name' of
    Nothing -> Nothing
    Just nammy ->
      case noNegative age' of
        Nothing -> Nothing
        Just agey ->
          case noNegative weight' of
            Nothing -> Nothing
            Just weighty ->
              Just (Cow nammy agey weighty)
cowFromString1 :: String
  -> Int -> Int -> Maybe Cow
cowFromString1 name' age' weight' =
  Cow <$> noEmpty name'
  <*> noNegative age'
  <*> noNegative weight'
cowFromString2 :: String
  -> Int -> Int -> Maybe Cow
cowFromString2 name' age' weight' =
  liftA3 Cow (noEmpty name')
  (noNegative age')
  (noNegative weight')
cow1 :: Maybe (Int -> Int -> Cow)
cow1 = Cow <$> noEmpty "Bess"
cow2 :: Maybe (Int -> Cow)
cow2 = cow1 <*> noNegative 1
cow3 :: Maybe Cow
cow3 = cow2 <*> noNegative 2
coww1 :: Applicative f => 
  f String -> f Int -> f Int -> f Cow
-- default: coww1 :: Maybe String -> 
--   Maybe Int -> Maybe Int -> Maybe Cow
coww1 = liftA3 Cow
coww2 :: Maybe Int -> Maybe Int -> Maybe Cow
coww2 = coww1 (noEmpty "blah")
coww3 :: Maybe Int -> Maybe Cow
coww3 = coww2 (noNegative 1)
coww4 :: Maybe Cow
coww4 = coww3 (noNegative 2)

-- Exercise: Fixer upper
------------------------
apply :: Maybe String 
apply = const <$> Just "Hello" <*> pure "World"
apply1 :: Maybe 
  (Integer, Integer, String, [Integer])
apply1 = (,,,) <$> Just 90 <*> Just 10 
  <*> Just "Tierness" <*> pure [1, 2, 3]

-- 17.6 Applicative laws
------------------------
-- Identity
-----------
test7 :: Either String Int
test7 = pure id <*> 
  (Right 8001::Either String Int)
test5 :: Bool
test5 = (fmap id [1..(5::Int)]) == 
  (pure id <*> [1..5])
-- Composition
--------------
test6 :: Bool
test6 = (pure (.) <*> [(+1)] <*> [(*2)] 
  <*> [1, 2, 3]) == ([(+1)] <*> ([(*2)] 
  <*> [1, 2, 3::Int]))
-- Homomorphism
---------------
test8 :: Bool
test8 = (pure (+1) <*> pure 1) == 
  (pure ((+1) 1) :: Either String Int)
-- Interchange
--------------
test9 :: Bool
test9 = (Just (+2) <*> pure 2 :: Maybe Int)
  == (pure ($ 2) <*> Just (+ 2))
test10 :: Bool
test10 = ([(+2), (*2)] <*> pure 1 :: [Int])
  == (pure ($ 1) <*> [(+2), (*2)])

-- LYAH Chapter 11 Section 11.2
-------------------------------
seqA :: (Applicative f) => 
  [f a] -> f [a]
seqA [] = pure []
seqA (x:xs) = (:) <$> x <*> seqA xs
check :: [Integer]
check = seqA [(+1)] 1
check2 :: [Integer]
check2 = ((:) <$> (+1) <*> pure []) 1
-- check3 = ((+(1:)) <*> pure []) 1
-- check4 = (+(1:[])) 1

test11 :: Integer
test11 = ((+) <$> (+3) <*> (*2)) 2






