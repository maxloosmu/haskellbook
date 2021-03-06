-- {-# LANGUAGE FlexibleContexts #-}
-- {-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
-- {-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving,
  DeriveTraversable, StandaloneDeriving #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Testing where

import Control.Applicative
  ( Applicative(liftA2), ZipList(ZipList),
  getZipList, Alternative )
import Data.Monoid ( Sum )
import Test.QuickCheck
    ( frequency, NonEmptyList,
      (==>), listOf1,
      within, Gen,
      quickCheck,
      Arbitrary(arbitrary),
      Property )
import Test.QuickCheck.Checkers
  ( eq, quickBatch, EqProp(..),
  TestBatch, Binop, leftId, rightId,
  isAssoc )
import Test.QuickCheck.Classes
  ( applicative, functor, monoid )
import Test.QuickCheck.Arbitrary ()
import Test.QuickCheck
  (NonEmptyList (..), property)
import Test.QuickCheck.Modifiers
  (NonEmptyList (..))

-- 17.7 You knew this was coming
--------------------------------
-- this is an attempt to make Bull
-- workable with Alll and Anyy, and
-- create Maeb
data Bull =
  Fools | Twoo
  deriving (Eq, Show)
newtype Alll = All2 { getAll2::Bull }
  deriving (Eq, Show)
newtype Anyy = Any2 { getAny2::Bull }
  deriving (Eq, Show)
data Maeb a =
  Nil | Juz a
  deriving (Eq, Show)
instance Semigroup Alll where
  All2 Fools <> All2 Fools = All2 Fools
  All2 Twoo <> All2 Twoo = All2 Twoo
  _ <> _ = All2 Fools
instance Semigroup Anyy where
  Any2 Fools <> Any2 Fools = Any2 Fools
  Any2 Twoo <> Any2 Twoo = Any2 Twoo
  _ <> _ = Any2 Twoo
instance Semigroup a =>
  Semigroup (Maeb a) where
    Nil <> a = a
    a <> Nil = a
    Juz a <> Juz b = Juz (a <> b)
instance Arbitrary Alll where
  arbitrary =
    frequency [ (1, return (All2 Fools)),
    (1, return (All2 Twoo)) ]
instance Arbitrary Anyy where
  arbitrary =
    frequency [ (1, return (Any2 Fools)),
    (1, return (Any2 Twoo)) ]
instance Arbitrary a =>
  Arbitrary (Maeb a) where
    arbitrary = do
      x <- arbitrary
      frequency [ (1, return Nil),
        (1, return (Juz x)) ]
instance Monoid Alll where
    mempty = All2 Twoo
    mappend = (<>)
instance Monoid Anyy where
    mempty = Any2 Fools
    mappend = (<>)
-- this is also possible:
-- instance Semigroup a =>
--   Monoid (Maeb a) where
instance Monoid a =>
  Monoid (Maeb a) where
    mempty = Nil
    mappend = (<>)
instance Functor Maeb where
  fmap _ Nil = Nil
  fmap f (Juz x) = Juz (f x)
instance Applicative Maeb where
  pure = Juz
  Nil <*> _ = Nil
  _ <*> Nil = Nil
  Juz f <*> Juz x = Juz (f x)
-- EqProp is from the checkers library
instance EqProp Alll where
  (=-=) = eq
instance EqProp Anyy where
  (=-=) = eq
instance Eq a => EqProp (Maeb a) where
  (=-=) = eq

-- 17.8 ZipList Monoid
----------------------
-- -- this cause mconcat to stack
-- -- overflow:
instance Semigroup a
  => Semigroup (ZipList a) where
    (<>) = liftA2 (<>)
instance Monoid a
  => Monoid (ZipList a) where
    mempty = pure mempty
    mappend = liftA2 mappend
    mconcat as =
      foldr mappend mempty as
-- -- copying from:
-- https://hackage.haskell.org/package/checkers-0.5.6/docs/src/Test.QuickCheck.Classes.html#monoid
monoidP :: forall a. (Monoid a, Show a,
  Arbitrary a, EqProp a) =>
  a -> TestBatch
monoidP = const ( "monoidP"
    , [ ("left  identity", leftId mappend (mempty :: a))
    , ("right identity", rightId mappend (mempty :: a))
    , ("associativity" , isAssoc (mappend :: Binop a))
    , ("mappend = (<>)", property monoidSemigroupP)
    , ("mconcat", property mconcatP)
      ]
    )
  where
    monoidSemigroupP :: a -> a -> Property
    monoidSemigroupP x y =
      mappend x y =-= x <> y
    -- mconcatP amended:
    mconcatP :: (EqProp a, Monoid a) =>
      NonEmptyList a -> Property
    mconcatP (NonEmpty as) = mconcat as =-=
      foldr mappend mempty as

zl :: ZipList (Sum Int)
zl = ZipList [1,1 :: Sum Int]
-- -- this is not necessary:
-- nonEmptyList :: Gen [[Int]]
-- nonEmptyList = listOf1
--   (arbitrary :: Gen [Int])

-- -- mconcat stack overflows here too,
-- -- not possible to resolve this from
-- -- mconcatP
-- mconcatP :: (EqProp b, Monoid b) =>
--   NonEmptyList b -> Property
-- mconcatP (getNonEmpty -> as) =
--   mconcat as =-=
--   foldr mappend mempty as

-- -- this is not necessary:
-- -- instance Arbitrary a
-- --   => Arbitrary (ZipList a) where
-- --     arbitrary = ZipList <$> arbitrary
-- -- instance Arbitrary a
-- --   => Arbitrary (Sum a) where
-- --     arbitrary = Sum <$> arbitrary
-- -- instance Eq a
-- --   => EqProp (ZipList a) where
-- --     (=-=) = eq

-- -- this also throws an error:
-- instance Semigroup a =>
--   Semigroup (ZipList a) where
--     ZipList [] <> ZipList ys = ZipList ys
--     ZipList xs <> ZipList [] = ZipList xs
--     ZipList (x:xs) <> ZipList (y:ys) =
--       ZipList (x <> y :
--       ZipList xs <> ZipList ys)
-- instance Semigroup a =>
--   Monoid (ZipList a) where
--     mempty = ZipList []

-- -- this is for testing if as == [] is a
-- -- problem, and it is a problem
-- mconcatP :: forall a.
--   (Eq a, EqProp a, Monoid a) =>
--   [a] -> Property
-- mconcatP as = within 1000 $ mconcat as =-=
--   foldr mappend mempty as
-- mconcatP' :: (Eq a, Monoid a) =>
--   [a] -> Property
-- mconcatP' as = as /= []  ==>
--   mconcat as == foldr mappend mempty as

-- -- this doesn't work because mconcat
-- -- test fail at first try
-- instance Semigroup a
--   => Semigroup (ZipList a) where
--     (<>) = liftA2 (<>)
-- instance (Eq a, Monoid a)
--   => Monoid (ZipList a) where
--     mempty = pure mempty
--     mappend = (<>)
--     mconcat as = if as /= [] then
--       foldr mappend mempty as
--       else ZipList []

-- -- developing newtype may not work:
-- newtype Ziplist a = Ziplst { getZiplst::a }
--   deriving (Eq, Ord, Show)
-- instance Semigroup (Ziplist a) where
--   Ziplst xs <> Ziplst [] = Ziplst xs
--   Ziplst [] <> Ziplst ys = Ziplst ys
--   Ziplst (x:xs) <> Ziplst (y:ys) =
--     Ziplst (x<>y : Ziplst xs <> Ziplst ys)

-- This works, Based on code from:
-- https://stackoverflow.com/questions/50130388/ziplist-monoid-haskell
-- and
-- https://stackoverflow.com/questions/65752398/haskell-quickbatch-testing-applicative-monoid-ziplist
newtype Ap f a = Ap { getAp :: f a }
  deriving (Eq, Show)
instance (Applicative f, Semigroup a) =>
  Semigroup (Ap f a) where
    Ap xs <> Ap ys =
      Ap $ liftA2 (<>) xs ys
instance (Applicative f, Monoid a) =>
  Monoid (Ap f a) where
    mempty = Ap $ pure mempty
    Ap xs `mappend` Ap ys =
      Ap $ liftA2 mappend xs ys
-- Ap $ liftA2 mappend xs ys ==
-- Ap $ mappend <$> xs <*> ys
-- type: Ap f a, where f a is considered
-- together because f is structure
app :: Ap ZipList (Sum Int)
app = Ap (ZipList [1,2 :: Sum Int])
test :: Ap ZipList (Sum Int)
test = app <> app
instance Arbitrary (f a) =>
  Arbitrary (Ap f a) where
    arbitrary = Ap <$> arbitrary
-- instance Eq a => EqProp (Ap ZipList a) where
--   xs =-= ys = xs' `eq` ys' where
--     xs' =
--       let (Ap (ZipList l)) = xs
--         in take 3000 l
--     ys' =
--       let l = (getZipList . getAp) ys
--         in take 3000 l

newtype MonZipList a =
  MonZipList (Ap ZipList a)
  deriving (Semigroup, Monoid, Functor,
  Applicative, Eq, Show)
deriving instance Functor f =>
  Functor (Ap f)
deriving instance Applicative f =>
  Applicative (Ap f)
monapp :: MonZipList (Sum Int)
monapp = MonZipList app
instance Arbitrary a =>
  Arbitrary (MonZipList a) where
    arbitrary = MonZipList <$> arbitrary
instance Eq a => EqProp (MonZipList a) where
  MonZipList (Ap (ZipList xs)) =-=
    MonZipList (Ap (ZipList ys)) =
    take 3000 xs `eq` take 3000 ys









main :: IO ()
main = do
  -- quickBatch (monoid (All2 Fools))
  -- quickBatch (monoid (Any2 Twoo))
  -- quickBatch $ functor
  --   (Juz (1,2,3) :: Maeb (Int, Int, Int))
  -- quickBatch $ applicative
  --   (undefined :: Maeb
  --   (Int, String, Maybe Char))
  quickBatch $ monoidP zl
  -- quickCheck (mconcatP @(ZipList (Sum Int)))
  -- quickCheck (mconcatP' @(ZipList (Sum Int)))
  -- -- monoid app goes with
  -- -- EqProp (Ap ZipList a):
  -- quickBatch $ monoid app
  -- quickBatch $ monoid monapp
  quickBatch $ monoid @(MonZipList (Sum Int))
    undefined
  quickBatch $ functor @MonZipList @Int @Bool
    @Char undefined
  quickBatch $ applicative @MonZipList @String
    @Int @Char undefined

