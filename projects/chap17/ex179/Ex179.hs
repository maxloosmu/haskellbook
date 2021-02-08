

module Ex179 where

import Data.Monoid
import Test.QuickCheck
import Test.QuickCheck.Checkers 
import Test.QuickCheck.Classes 

-- pure :: a -> [a]
-- (<*>) :: [a -> b] -> [a] -> [b]
-- pure :: a -> IO a
-- (<*>) :: IO (a -> b) -> IO a -> IO b
-- pure :: a -> (a,a)
-- (<*>) :: (a,a->b) -> (a,a) -> (a,b)
-- pure :: a -> (e -> a)
-- (<*>) :: (e->(a->b)) -> (e->a) -> (e->b)

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
test = Two (Sum 1) (+2) <*> Two (Sum 1) 2
instance (Arbitrary a, Arbitrary b) => 
  Arbitrary (Two a b) where
    arbitrary = do
      x <- arbitrary
      y <- arbitrary
      return (Two x y)
instance (Eq a, Eq b) => 
  EqProp (Two a b) where
    (=-=) = eq



main :: IO ()
main = do 
  -- quickBatch $ functor (undefined ::
  --   Pair (Int, Bool, Char))
  -- quickBatch $ applicative (undefined ::
  --   Pair (Int, Bool, Char))
  quickBatch $ functor (undefined ::
    Two Int (Int, Bool, Char))
  quickBatch $ applicative (undefined ::
    Two (Sum Int) (Int, Bool, Char))
