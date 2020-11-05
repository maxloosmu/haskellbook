module Addition where

import Test.Hspec 
  ( hspec, describe, it, shouldBe, Expectation )
import Test.QuickCheck
    ( choose,
      elements,
      frequency,
      sample,
      sample',
      quickCheck,
      Arbitrary(arbitrary),
      Gen,
      Testable(property) )

sayHello :: IO ()
sayHello = putStrLn "hello!"

main :: IO ()
main = hspec $ do
  describe "Addition" $ do
    it "1 + 1 is greater than 1" $ do
      ((1 :: Integer) + 1) > 1 `shouldBe` 
        True :: Expectation
    it "2 + 2 is equal to 4" $ do
      (2 :: Integer) + 2 `shouldBe` 4
    it "15 divided by 3 is 5" $ do
      dividedBy (15 :: Integer) 3 `shouldBe` (5, 0)
    it "22 divided by 5 is\
      \ 4 remainder 2" $ do
        dividedBy (22 :: Integer) 5 `shouldBe` (4, 2)
    it "2 multiply 3 is 6" $ do
      multBy (2 :: Integer) (3 :: Integer) `shouldBe` 6
    it "x + 1 is always\
      \ greater than x" $ do
        property $ \x -> x + 1 > (x :: Int)

dividedBy :: Integral a => a -> a -> (a, a)
dividedBy num denom = go num denom 0 where 
  go n d count
    | n < d = (count, n)
    | otherwise =
      go (n - d) d (count + 1)
multBy :: (Eq t1, Num t1, Num t2) => t1 -> t2 -> t2
multBy x y = go x y 0 where
  go x1 y1 summ
    | x1 == 0 = summ
    | otherwise = go (x1 - 1) y1 (summ + y1)

test :: IO ()
test = sample (arbitrary :: Gen Char)

trivialInt :: Gen Int
trivialInt = return 1
test2 = sample' genChar
oneThroughThree :: Gen Int
oneThroughThree = elements [1, 2,2,2,2, 3]
genBool :: Gen Bool
genBool = choose (False, True)
genBool' :: Gen Bool
genBool' = elements [False, True]
genOrdering :: Gen Ordering
genOrdering = elements [LT, EQ, GT]
genChar :: Gen Char
genChar = elements ['a'..'z']


genTuple :: (Arbitrary a, Arbitrary b)
  => Gen (a, b)
genTuple = do
  a <- arbitrary
  b <- arbitrary
  return (a, b)
genThreeple :: (Arbitrary a, Arbitrary b,
  Arbitrary c)
  => Gen (a, b, c)
genThreeple = do
  a <- arbitrary
  b <- arbitrary
  c <- arbitrary
  return (a, b, c)
genFourple :: (Arbitrary a, Arbitrary b,
  Arbitrary c, Arbitrary d)
  => Gen (a, b, c, d)
genFourple = do
  a <- arbitrary
  b <- arbitrary
  c <- arbitrary
  d <- arbitrary
  return (a, b, c, d)
type G1 = Gen (Int, Float)
type G2 = Gen ([()], Char)
type G3 = Gen (Int, Float, [()], String)
test3 :: IO ()
test3 = sample (genFourple :: G3)

type G4 = Gen (Either Int Float)
type G5 = Gen (Maybe Char)
genEither :: (Arbitrary a, Arbitrary b)
  => Gen (Either a b)
genEither = do
  a <- arbitrary
  b <- arbitrary
  elements [Left a, Right b]
genMaybe :: Arbitrary a => Gen (Maybe a)
genMaybe = do
  a <- arbitrary
  elements [Nothing, Just a]
genMaybe' :: Arbitrary a => Gen (Maybe a)
genMaybe' = do
  a <- arbitrary
  frequency 
    [ (1, return Nothing)
    , (3, return (Just a))]
test4 :: IO ()
test4 = sample (genEither :: G4)
test5 :: IO ()
test5 = sample (genMaybe' :: G5)

prop_additionGreater :: Int -> Bool
prop_additionGreater x = x + 1 > x
runQc :: IO ()
runQc = quickCheck prop_additionGreater



