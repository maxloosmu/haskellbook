-- {-# LANGUAGE FlexibleContexts #-}
-- {-# LANGUAGE UndecidableInstances #-}
-- {-# LANGUAGE RankNTypes #-}
-- {-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleInstances #-}

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
  (<*>) fs xs = flatMap (<$> fs) xs

-- (<*>)::List(a->b)->List a->
-- flatMap::(a->List b)->List a->List b
-- fmap::(a->b)->List a->List b
-------------------------------
-- ZipList Applicative exercise
-------------------------------












-- main :: IO ()
-- main = do 
  -- quickBatch $ monoid v2
  