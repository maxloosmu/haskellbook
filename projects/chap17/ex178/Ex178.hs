{-# LANGUAGE FlexibleContexts #-}
-- {-# LANGUAGE UndecidableInstances #-}
-- {-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -Wno-type-defaults #-}
module Ex178 where

import Control.Applicative
  ( Applicative(liftA2), ZipList(ZipList) )
import Data.Monoid ( Sum )
import Test.QuickCheck
    ( frequency, Gen,
      (==>),
      within,
      quickCheck,
      Arbitrary(arbitrary),
      Property )
import Test.QuickCheck.Checkers
  ( eq, quickBatch, EqProp(..) )
import Test.QuickCheck.Classes
  ( applicative, functor, monoid )
import Test.QuickCheck.Arbitrary ()

-- 17.8 ZipList Monoid
----------------------
-- List Applicative exercise
----------------------------
-- -- this applicative creates monoid
-- -- type outputs, which is wrong
-- -- for Applicatives. trying
-- -- Cons (f x) (fs <> xs) using
-- -- Monoid fail because errorneous
-- -- output for (f x). trying with
-- -- Cons2 (x <> y) (xs <> ys) also
-- -- fails, see below:
-- instance Applicative List where
--   pure x = Cons x Nil
--   (<*>) _ Nil = Nil
--   (<*>) Nil _ = Nil
--   (Cons f fs) <*> (Cons x xs) =
--     Cons (f x) (fs <*> xs)
data Ziplst a =
  Nil2 | Cons2 a (Ziplst a)
  deriving (Eq, Show)
newtype Sum1 = Sum2 Int
  deriving (Eq, Show)
instance Semigroup Sum1 where
  (<>) (Sum2 x) (Sum2 y) =
    Sum2 (x + y)
instance Monoid Sum1 where
  mempty = Sum2 0
  mappend = (<>)
instance Arbitrary Sum1 where
  arbitrary = do
    x <- arbitrary :: Gen Int
    return $ Sum2 x
-- Semigroup (Ziplst a) cannot
-- work because (x <> y) will
-- recurse endlessly, so
-- quickBatch $ monoid v2 hangs
-- at left identity
instance Semigroup a =>
  Semigroup (Ziplst a) where
    (<>) _ Nil2 = Nil2
    (<>) Nil2 _ = Nil2
    (Cons2 x xs) <> (Cons2 y ys) =
      Cons2 (x <> y) (xs <> ys)
instance Monoid a =>
  Monoid (Ziplst a) where
    mempty = Nil2
    mappend = (<>)
-- no other solution for:
-- arbitrary = arbitrary
instance Arbitrary a =>
  Arbitrary (Ziplst a) where
    arbitrary = arbitrary
-- -- "Cons2 arbitrary.." don't work:
-- -- Expected type: Gen (Gen a0)
-- -- Actual type: Ziplst (Gen a0)
-- instance Arbitrary a =>
--   Arbitrary (Ziplst a) where
--     arbitrary = do
--       x <- Cons2 arbitrary
--         (Cons2 arbitrary Nil2)
--       return x
-- -- zlistGen don't work too:
-- zlistGen :: Arbitrary a =>
--   Gen (Ziplst a)
-- zlistGen = do
--   x <- arbitrary
--   frequency [(1, return Nil2),
--     (10, Cons2 x zlistGen)]
-- instance Arbitrary a =>
--   Arbitrary (Ziplst a) where
--     arbitrary = zlistGen
instance EqProp (Ziplst Sum1) where
  (=-=) = eq
v2 :: Ziplst Sum1
v2 = Cons2 (Sum2 1)
  (Cons2 (Sum2 2) Nil2)
----------------------------------
----------------------------------
-- List Applicative exercise -
-- the correct version
----------------------------------
data List a =
  Nil | Cons a (List a)
  deriving (Eq, Show)
f1 :: List (Integer -> Integer)
f1 = Cons (+1) (Cons (*2) Nil)
v1 :: List Integer
v1 = Cons 1 (Cons 2 Nil)
append :: List a -> List a -> List a
append Nil ys = ys
append (Cons x xs) ys =
  Cons x $ xs `append` ys
fold :: (a -> b -> b) ->
  b -> List a -> b
fold _ b Nil = b
fold f b (Cons h t) =
  f h (fold f b t)
concat' :: List (List a) -> List a
concat' = fold append Nil
flatMap :: (a -> List b)
  -> List a -> List b
flatMap f as = concat' $ fmap f as
-- fv2 output: Cons 2 (Cons 3 Nil)
fv2 :: List Integer
fv2 = fmap (+1) v1
v3 :: List (List Integer)
v3 = Cons (Cons 2 Nil)
  (Cons (Cons 3 Nil) Nil)
-- fv3 output: Cons 1 (Cons 2
-- (Cons 1 (Cons 3 Nil)))
fv3 :: List Integer
fv3 = flatMap (Cons 1) v3
-- test output: [[1,2],[1,3]]
test :: [[Integer]]
test = fmap (1:) [[2],[3]]
toMyList :: [a] -> List a
toMyList = foldr Cons Nil
xs2 :: List Integer
xs2 = toMyList [1, 2]
c :: a -> List a -> List a
c = Cons
f2 :: Num a => a -> List a
f2 x = x `c` (9 `c` Nil)
-- test2 output:
-- Cons 1 (Cons 9 (Cons 2
-- (Cons 9 Nil)))
test2 :: List Integer
test2 = flatMap f2 xs2
-- fv1 :: List Integer
-- fv1 = f1 <*> v1
instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons x xs) =
    Cons (f x) (fmap f xs)
instance Applicative List where
  pure x = Cons x Nil
  (<*>) _ Nil = Nil
  (<*>) Nil _ = Nil
  (<*>) fs xs = flatMap (<$> xs) fs
-- `flatMap (<$> xs) fs` works because:
-- [f g] <*> [x y]
-- == [f x, f y, g x, g y]
-- == concat [ [f x, f y] , [g x, g y] ]
-- == concat [ f <$> [x, y] , g <$> [x, y] ]
-- == concat $ map (<$> [x, y]) [f, g]
instance Arbitrary a =>
  Arbitrary (List a) where
    arbitrary = listGen2
-- Cons x <$> listGen works because we're
-- lifting the Gen in listGen
listGen :: Arbitrary a => Gen (List a)
listGen = do
  x <- arbitrary
  frequency [(1, return Nil),
    (10, Cons x <$> listGen)]
listGen2 :: Arbitrary a => Gen (List a)
listGen2 = do
  x <- arbitrary
  frequency [(1, return Nil),
    (10, listGen2 >>= (\xs ->
      return (Cons x xs)))]
listGen3 :: Arbitrary a => Gen (List a)
listGen3 = do
  frequency [(1, return Nil),
    (10, listGen4)] where
      listGen4 = do
        x <- arbitrary
        xs <- listGen3
        return (Cons x xs)

instance Eq a => EqProp (List a) where
  (=-=) = eq
-- (<*>)::List(a->b)->List a->List b
-- flatMap::(a->List b)->List a->List b
-- fmap::(a->b)->List a->List b
-- c::a->List a->List a
-------------------------------
-- ZipList Applicative exercise
-------------------------------
newtype ZipList' a =
  ZipList' (List a)
  deriving (Eq, Show)
instance Arbitrary a => Arbitrary (ZipList' a) where
  arbitrary = ZipList' <$> arbitrary
instance Eq a => EqProp (ZipList' a) where
  xs =-= ys = xs' `eq` ys'
    where
      xs' =
        let (ZipList' l) = xs
        in take1 3000 l
      ys' =
        let (ZipList' l) = ys
        in take1 3000 l
take1 :: Int -> List a -> List a
take1 n xs
  | n > 0 = take2 n xs
  | otherwise = Nil
take2 :: Int -> List a -> List a
take2 0 _ = Nil
take2 _ Nil = Nil
take2 n (Cons x xs) = Cons x (take1 (n-1) xs)
instance Functor ZipList' where
  fmap f (ZipList' xs) = ZipList' (fmap f xs)
repeat2 :: t -> List t
repeat2 x = Cons x (repeat2 x)
zip2 :: List (t -> a) -> List t -> List a
zip2 Nil _ = Nil
zip2 _ Nil = Nil
zip2 (Cons f fs) (Cons x xs) =
  Cons (f x) $ zip2 fs xs
instance Applicative ZipList' where
  pure x = ZipList' (repeat2 x)
  ZipList' Nil <*> _ = ZipList' Nil
  _ <*> ZipList' Nil = ZipList' Nil
  ZipList' (Cons f fs) <*>
    ZipList' (Cons x xs) =
      ZipList' $ Cons (f x) (zip2 fs xs)
zl' :: [a] -> ZipList' a
zl' = ZipList' . toMyList
z :: ZipList' (Integer -> Integer)
z = zl' [(+9), (*2), (+8)]
z1 :: ZipList' Integer
z1 = zl' [1..3]
test3 :: ZipList' Integer
test3 = z <*> z1
-- test3 output: ZipList'
-- (Cons 10 (Cons 4 (Cons 11 Nil)))
z2 :: ZipList' Integer
z2 = pure 1
test4 :: ZipList' Integer
test4 = z <*> z2
-- test4 output: ZipList'
-- (Cons 10 (Cons 2 (Cons 9 Nil)))
z3 :: ZipList' Integer
z3 = zl' [1, 2]
test5 :: ZipList' Integer
test5 = pure id <*> z3
------------------------
-- Either vs. Validation
------------------------
data Validation err a =
  Failure err
  | Success a
  deriving (Eq, Show)
validationToEither :: (Eq e, Eq a) =>
  Validation e a
  -> Either e a
validationToEither (Failure err) = Left err
validationToEither (Success a) = Right a
eitherToValidation :: (Eq e, Eq a) =>
  Either e a
  -> Validation e a
eitherToValidation (Left err) = Failure err
eitherToValidation (Right a) = Success a
-- -- cannot test directly because cannot
-- -- fit an err type into it
-- test6 :: Bool
-- test6 = (eitherToValidation .
--   validationToEither)
--   (Success 1 :: Validation err Int)
--   == id (Success 1)
instance (Arbitrary e, Arbitrary a) =>
  Arbitrary (Validation e a) where
    arbitrary = do
      e <- arbitrary
      a <- arbitrary
      frequency [(1, return $ Failure e),
        (2, return $ Success a)]
-- -- test6 (Success 1::Eq err=>Validation err Int)
-- -- still throws warnings
test6 :: (Eq e, Eq a) =>
  Validation e a -> Bool
test6 x = (eitherToValidation .
  validationToEither) x == id x

data Errors =
  DividedByZero
  | StackOverflow
  | MooglesChewedWires
  deriving (Eq, Show)
instance Functor (Validation e) where
  fmap _ (Failure x) = Failure x
  fmap f (Success y) = Success (f y)
instance Monoid e =>
  Applicative (Validation e) where
    pure = Success
    Failure e1 <*> Failure e2
      = Failure (e1 <> e2)
    Failure e <*> _ = Failure e
    _ <*> Failure e = Failure e
    Success f <*> Success r = Success (f r)
-- [Errors] works because the e in
-- Validation e a can be any type
success :: Validation [Errors] Int
success = Success (+1)
  <*> Success (1::Int)
fail0 :: Validation [Errors] Int
fail0 = Success (+1)
  <*> Failure [StackOverflow::Errors]
-- fail1a works only with *> in fail5:
fail1a :: Validation [Errors] Int
fail1a = Failure [StackOverflow::Errors]
  <*> Success (1)
-- fail1b works with either *> or <*>
-- in fail4:
fail1b :: Validation [Errors] b
fail1b = Failure [StackOverflow::Errors]
  <*> Success (1::Int)
fail2 :: Validation [Errors] Int
fail2 = Failure [MooglesChewedWires]
  <*> Failure [StackOverflow::Errors]
fail3 :: Validation [Errors] Int
fail3 = (Failure [StackOverflow::Errors]
  <*> Success (+1))
  <*> (Failure [MooglesChewedWires]
  <*> Failure [StackOverflow::Errors])
fail4 :: Validation [Errors] Int
fail4 = fail1b <*> fail2
fail5 :: Validation [Errors] Int
fail5 = fail1a *> fail2
fail6 :: Validation [Errors] Int
fail6 = fail2 *> success
-- succ1 output: Success 2
succ1 :: Validation [Errors] Int
succ1 = success *> success
-- succ2 output: Success 4
succ2 :: Validation [Errors] Int
succ2 = (+) <$> success <*> success


main :: IO ()
main = do
  -- quickBatch $ monoid v2
  -- quickBatch $ functor (undefined ::
  --   List (Int, Bool, Char))
  -- quickBatch $ applicative
  --   @List @Int @Bool @Char undefined
  -- quickBatch $ functor (undefined ::
  --   ZipList' (Int, Bool, Char))
  -- quickBatch $ applicative (undefined ::
  --   ZipList' (Int, Bool, Char))
  quickCheck (test6 ::
    Validation [String] Int -> Bool)
