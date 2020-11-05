module Arbinst where

import Test.QuickCheck 
  ( sample, Arbitrary(arbitrary), Gen, frequency )
import Test.QuickCheck.Gen (oneof)

data Trivial =
  Trivial
  deriving (Eq, Show)
trivialGen :: Gen Trivial
trivialGen =
  return Trivial
instance Arbitrary Trivial where
  arbitrary = trivialGen
main :: IO ()
main = do
  sample trivialGen
  sample (identityGen :: 
    Gen (Identity String))
  sample (pairGen :: 
    Gen (Pair Float Char))
  sample (sumGenEqual ::
    Gen (Sum Int [()]))
  sample (maybeGen ::
    Gen (Maybe Int))
  sample sumGenCharIntFirst

data Identity a =
  Identity a
  deriving (Eq, Show)
identityGen :: Arbitrary a =>
  Gen (Identity a)
identityGen = do
  a <- arbitrary
  return (Identity a)
instance Arbitrary a =>
  Arbitrary (Identity a) where
    arbitrary = identityGen
identityGenInt :: Gen (Identity Int)
identityGenInt = identityGen

data Pair a b =
  Pair a b
  deriving (Eq, Show)
pairGen :: (Arbitrary a,
  Arbitrary b) =>
  Gen (Pair a b)
pairGen = do
  a <- arbitrary
  b <- arbitrary
  return (Pair a b)
instance (Arbitrary a, 
  Arbitrary b) =>
  Arbitrary (Pair a b) where
    arbitrary = pairGen

data Sum a b =
  First a
  | Second b
  deriving (Eq, Show)
sumGenEqual :: (Arbitrary a,
  Arbitrary b) =>
  Gen (Sum a b)
sumGenEqual = do
  a <- arbitrary
  b <- arbitrary
  oneof [return $ First a, 
    return $ Second b]
instance (Arbitrary a, 
  Arbitrary b) =>
  Arbitrary (Sum a b) where
    arbitrary = sumGenEqual

maybeGen :: Arbitrary a =>
  Gen (Maybe a)
maybeGen = do
  a <- arbitrary
  frequency [(3 :: Int, return $ Just a), 
    (1, return Nothing)]
-- -- this is duplicate of instance in 
-- -- Test.QuickCheck.Arbitrary, 
-- -- cannot duplicate:
-- instance Arbitrary a =>
--   Arbitrary (Maybe a) where
--     arbitrary = frequency [(1, return Nothing),
--       (3, liftM Just arbitrary)]

sumGenFirstPls :: (Arbitrary a,
  Arbitrary b) =>
  Gen (Sum a b)
sumGenFirstPls = do
  a <- arbitrary
  b <- arbitrary
  frequency [(10, return $ First a),
    (1, return $ Second b)]
sumGenCharIntFirst :: Gen (Sum Char Int)
sumGenCharIntFirst = sumGenFirstPls

