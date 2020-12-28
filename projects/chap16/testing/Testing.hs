module Testing where

import Test.QuickCheck 

----- 16.10 Exercises: Instances of Func
----------------------------------------
functorIdentity :: (Functor f, Eq (f a)) =>
  f a -> Bool
functorIdentity f =
  fmap id f == f
-- fmap applied to id will only lift and 
-- put down functor [] without changing the 
-- insides, test output: [1]
test :: [Integer]
test = fmap id [1]
f2 :: [Int] -> Bool
f2 x = functorIdentity x

functorCompose :: (Eq (f c), Functor f) =>
  (a -> b) -> (b -> c) -> f a -> Bool
functorCompose f1 g x =
  fmap g (fmap f1 x) == fmap (g . f1) x
c :: [Int] -> Bool
c x = functorCompose (+1) (*2) 
  (x :: [Int])

newtype Identity a = Identity a
  deriving (Eq, Show)
-- instance Functor Identity required
-- for functorCompose
instance Functor Identity where
  fmap f (Identity x) = Identity (f x)
instance Arbitrary a => 
  Arbitrary (Identity a) where
    arbitrary = do
      x <- arbitrary
      return (Identity x)
f3 :: Identity Int -> Bool
f3 (Identity x) = functorIdentity 
  (Identity x)
c1 :: Identity Int -> Bool
c1 (Identity x) = functorCompose (+1) (*2) 
  (Identity x :: Identity Int)

data Pair a = Pair a a
  deriving (Eq, Show)
instance Functor Pair where
  fmap f (Pair x y) = Pair (f x) (f y)
instance Arbitrary a => 
  Arbitrary (Pair a) where
    arbitrary = do
      x <- arbitrary
      y <- arbitrary
      return (Pair x y)
f4 :: Pair Int -> Bool
f4 (Pair x y) = functorIdentity (Pair x y)
c2 :: Pair Int -> Bool
c2 (Pair x y) = functorCompose (+1) (*2) 
  (Pair x y :: Pair Int)

-- for Two a b, need to change kind 
-- to * -> * from * -> * -> *, so 
-- consider (Two a)
data Two a b = Two a b
  deriving (Eq, Show)
instance Functor (Two a) where
  fmap f (Two x y) = Two x (f y)
instance (Arbitrary a, Arbitrary b) => 
  Arbitrary (Two a b) where
    arbitrary = do
      x <- arbitrary
      y <- arbitrary
      return (Two x y)
f5 :: Two String Int -> Bool
f5 (Two x y) = functorIdentity (Two x y)
c3 :: Two String Int -> Bool
c3 (Two x y) = functorCompose (+1) (*2) 
  (Two x y :: Two String Int)

data Three a b c = Three a b c
  deriving (Eq, Show)
instance Functor (Three a b) where
  fmap f (Three x y z) = Three x y (f z)
instance (Arbitrary a, Arbitrary b, 
  Arbitrary c) => 
    Arbitrary (Three a b c) where
      arbitrary = do
        x <- arbitrary
        y <- arbitrary
        z <- arbitrary
        return (Three x y z)
f6 :: Three Char String Int -> Bool
f6 (Three x y z) = functorIdentity 
  (Three x y z)
c4 :: Three Char String Int -> Bool
c4 (Three x y z) = functorCompose (+1) (*2) 
  (Three x y z :: Three Char String Int)

data Three' a b = Three' a b b
  deriving (Eq, Show)
instance Functor (Three' a) where
  fmap f (Three' x y z) = Three' x (f y) (f z)
instance (Arbitrary a, Arbitrary b) => 
    Arbitrary (Three' a b) where
      arbitrary = do
        x <- arbitrary
        y <- arbitrary
        z <- arbitrary
        return (Three' x y z)
f7 :: Three' Char Int -> Bool
f7 (Three' x y z) = functorIdentity 
  (Three' x y z)
c5 :: Three' Char Int -> Bool
c5 (Three' x y z) = functorCompose (+1) (*2) 
  (Three' x y z :: Three' Char Int)

data Four' a b = Four' a a a b
  deriving (Eq, Show)
instance Functor (Four' a) where
  fmap f (Four' w x y z) = Four' w x y (f z)
instance (Arbitrary a, Arbitrary b) => 
    Arbitrary (Four' a b) where
      arbitrary = do
        w <- arbitrary
        x <- arbitrary
        y <- arbitrary
        z <- arbitrary
        return (Four' w x y z)
f8 :: Four' Char Int -> Bool
f8 (Four' w x y z) = functorIdentity 
  (Four' w x y z)
c6 :: Four' Char Int -> Bool
c6 (Four' w x y z) = functorCompose (+1) (*2) 
  (Four' w x y z :: Four' Char Int)



main :: IO ()
main = do 
  -- quickCheck f2
  -- quickCheck c
  quickCheck f3
  quickCheck c1
  quickCheck f4
  quickCheck c2
  quickCheck f5
  quickCheck c3
  quickCheck f6
  quickCheck c4
  quickCheck f7
  quickCheck c5
  quickCheck f8
  quickCheck c6




