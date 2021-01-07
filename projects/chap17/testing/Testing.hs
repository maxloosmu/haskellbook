-- {-# LANGUAGE FlexibleContexts #-}
-- {-# LANGUAGE UndecidableInstances #-}

module Testing where

import Control.Applicative
  ( Applicative(liftA2), ZipList(ZipList) )
import Data.Monoid ( Sum )
import Test.QuickCheck 
  ( frequency, Arbitrary(arbitrary) ) 
import Test.QuickCheck.Checkers 
  ( eq, quickBatch, EqProp(..) )
import Test.QuickCheck.Classes 
  ( applicative, functor, monoid ) 
import Test.QuickCheck.Arbitrary ()


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
main :: IO ()
main = do 
  quickBatch (monoid (All2 Fools))
  quickBatch (monoid (Any2 Twoo))
  quickBatch $ functor 
    (Juz (1,2,3) :: Maeb (Int, Int, Int))
  quickBatch $ applicative 
    (undefined :: Maeb (Int, String, Maybe Char))
  quickBatch $ monoid zl

instance Semigroup a 
  => Semigroup (ZipList a) where
    (<>) = liftA2 (<>)
instance Monoid a
  => Monoid (ZipList a) where
    mempty = pure mempty 
    mappend = liftA2 mappend
-- instance Arbitrary a
--   => Arbitrary (ZipList a) where
--     arbitrary = ZipList <$> arbitrary
-- instance Arbitrary a
--   => Arbitrary (Sum a) where
--     arbitrary = Sum <$> arbitrary
-- instance Eq a
--   => EqProp (ZipList a) where
--     (=-=) = eq
zl :: ZipList (Sum Int)
zl = ZipList [1,1 :: Sum Int]







