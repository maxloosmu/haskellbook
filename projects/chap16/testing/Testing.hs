module Testing where

import Test.QuickCheck 

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
  (a -> b)
  -> (b -> c)
  -> f a
  -> Bool
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
f3 x = functorIdentity x
c1 :: Identity Int -> Bool
c1 x = functorCompose (+1) (*2) 
  (x :: Identity Int)


main :: IO ()
main = do 
  -- quickCheck f2
  -- quickCheck c
  quickCheck f3
  quickCheck c1
