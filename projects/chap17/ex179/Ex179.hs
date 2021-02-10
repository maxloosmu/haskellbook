

module Ex179 where

import Data.Monoid
import Control.Applicative (liftA3)
import Test.QuickCheck
import Test.QuickCheck.Checkers 
import Test.QuickCheck.Classes 

-- pure :: a -> [a]
-- (<*>) :: [a -> b] -> [a] -> [b]
-- pure :: a -> IO a
-- (<*>) :: IO (a->b) -> IO a -> IO b
-- pure :: a -> (a,a)
-- (<*>) :: (a,a->b) -> (a,a) -> (a,b)
-- pure :: a -> (e -> a)
-- (<*>) :: (e->(a->b)) -> 
--   (e->a) -> (e->b)

data Pair a = Pair a a 
  deriving Show
instance Functor Pair where
  fmap f (Pair x y) = Pair (f x) (f y)
instance Applicative Pair where
  pure x = Pair x x
  (<*>) (Pair f1 f2) (Pair x1 x2) = 
    Pair (f1 x1) (f2 x2)
instance Arbitrary a => 
  Arbitrary (Pair a) where
    arbitrary = do
      x <- arbitrary
      y <- arbitrary
      return (Pair x y)
instance Eq a => EqProp (Pair a) where
  (Pair x1 x2) =-= (Pair y1 y2) = 
    (x1, x2) `eq` (y1, y2)

data Two a b = Two a b
  deriving (Show, Eq)
instance Functor (Two a) where
  fmap f (Two x y) = Two x (f y) 
instance Monoid a => 
  Applicative (Two a) where
    pure x = Two mempty x
    (<*>) (Two f1 f2) (Two x1 x2) =
      Two (f1 <> x1) (f2 x2)
test :: Two (Sum Int) Int
test = Two (Sum 1) (+2) <*> 
  Two (Sum 1) 2
instance (Arbitrary a, Arbitrary b) => 
  Arbitrary (Two a b) where
    arbitrary = do
      x <- arbitrary
      y <- arbitrary
      return (Two x y)
instance (Eq a, Eq b) => 
  EqProp (Two a b) where
    (=-=) = eq

data Three a b c = Three a b c
  deriving (Eq, Show)
instance Functor (Three a b) where
  fmap f (Three x y z) = 
    Three x y (f z)
instance (Monoid a, Monoid b) => 
  Applicative (Three a b) where
    pure x = Three mempty mempty x
    (<*>) (Three l1 l2 f) 
      (Three r1 r2 x) =
      Three (l1 <> r1) (l2 <> r2) 
      (f x)
instance (Arbitrary a, Arbitrary b, 
  Arbitrary c) => 
    Arbitrary (Three a b c) where
      arbitrary = do
        x <- arbitrary
        y <- arbitrary
        z <- arbitrary
        return (Three x y z)
instance (Eq a, Eq b, Eq c) => 
  EqProp (Three a b c) where
    (=-=) = eq

data Three2 a b = Three2 a b b
  deriving (Show, Eq)
instance Functor (Three2 a) where
  fmap f (Three2 x y z) = 
    Three2 x (f y) (f z)
instance Monoid a => 
  Applicative (Three2 a) where
    pure x = Three2 mempty x x
    (<*>) (Three2 l f1 f2) 
      (Three2 r x1 x2) =
      Three2 (l <> r) (f1 x1) (f2 x2)
instance (Arbitrary a, Arbitrary b) =>
    Arbitrary (Three2 a b) where
      arbitrary = do
        x <- arbitrary
        y <- arbitrary
        z <- arbitrary
        return (Three2 x y z)
instance (Eq a, Eq b) => 
  EqProp (Three2 a b) where
    (=-=) = eq

data Four a b c d = Four a b c d
  deriving (Show, Eq)
instance Functor (Four a b c) where
  fmap f (Four w x y z) = 
    Four w x y (f z)
instance (Monoid a, Monoid b, 
  Monoid c) =>
  Applicative (Four a b c) where
    pure = Four mempty mempty mempty
    (<*>) (Four l1 l2 l3 f) 
      (Four r1 r2 r3 x) = 
      Four (l1 <> r1) (l2 <> r2)
      (l3 <> r3) (f x)
instance (Arbitrary a, Arbitrary b,
  Arbitrary c, Arbitrary d) =>
  Arbitrary (Four a b c d) where
    arbitrary = do
      w <- arbitrary
      x <- arbitrary
      y <- arbitrary
      z <- arbitrary
      return $ Four w x y z
instance (Eq a, Eq b, Eq c, Eq d) =>
  EqProp (Four a b c d) where
    (=-=) = eq

data Four2 a b  = Four2 a a a b  
  deriving (Show, Eq)
instance Functor (Four2 a) where
  fmap f (Four2 w x y z) = 
    Four2 w x y (f z)
instance Monoid a =>
  Applicative (Four2 a) where
    pure = Four2 mempty mempty mempty
    (<*>) (Four2 l1 l2 l3 f) 
      (Four2 r1 r2 r3 x) = 
      Four2 (l1 <> r1) (l2 <> r2)
      (l3 <> r3) (f x)
instance (Arbitrary a, Arbitrary b)
  => Arbitrary (Four2 a b) where
    arbitrary = do
      w <- arbitrary
      x <- arbitrary
      y <- arbitrary
      z <- arbitrary
      return $ Four2 w x y z
instance (Eq a, Eq b) =>
  EqProp (Four2 a b) where
    (=-=) = eq

main :: IO ()
main = do 
  -- quickBatch $ functor (undefined ::
  --   Pair (Int, Bool, Char))
  -- quickBatch $ applicative (undefined ::
  --   Pair (Int, Bool, Char))
  -- quickBatch $ functor (undefined ::
  --   Two Int (Int, Bool, Char))
  -- quickBatch $ applicative (undefined ::
  --   Two (Sum Int) (Int, Bool, Char))
  -- quickBatch $ functor (undefined ::
  --   Three Int Int (Int, Bool, Char))
  -- quickBatch $ applicative (undefined ::
  --   Three (Sum Int) (Sum Int) 
  --   (Int, Bool, Char))
  -- quickBatch $ functor (undefined ::
  --   Three2 Int (Int, Bool, Char))
  -- quickBatch $ applicative (undefined ::
  --   Three2 (Sum Int)  
  --   (Int, Bool, Char))
  quickBatch $ functor (undefined ::
    Four Int Int Int (Int, Bool, Char))
  quickBatch $ applicative (undefined ::
    Four (Sum Int) (Sum Int) (Sum Int)
    (Int, Bool, Char))
  quickBatch $ functor (undefined ::
    Four2 Int (Int, Bool, Char))
  quickBatch $ applicative (undefined ::
    Four (Sum Int) (Sum Int) (Sum Int) 
    (Int, Bool, Char))
---------------
-- Combinations
---------------
stops :: String
stops = "pbtdkg"
vowels :: String
vowels = "aeiou"
combos :: [a] -> [b] -> [c] -> [(a, b, c)]
combos = liftA3 (,,)
test2 :: [(Char, Char, Char)]
test2 = combos stops vowels stops


