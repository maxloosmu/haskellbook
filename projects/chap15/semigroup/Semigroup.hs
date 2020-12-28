{-  -}

module Semigroup where

import Data.Monoid
import Test.QuickCheck 
import Data.Semigroup

data Trivial = Trivial deriving (Eq, Show)
instance Semigroup Trivial where
  _ <> _ = Trivial
instance Arbitrary Trivial where
  arbitrary = return Trivial
type TrivAssoc =
  Trivial -> Trivial -> Trivial -> Bool

semigroupAssoc :: (Eq m, Semigroup m)
  => m -> m -> m -> Bool
semigroupAssoc a b c =
  (a <> (b <> c)) == ((a <> b) <> c)

mli :: (Eq m, Monoid m)
  => m -> Bool
mli a = (mempty <> a) == a
mri :: (Eq m, Monoid m)
  => m -> Bool
mri a = (a <> mempty) == a

main :: IO ()
main = do
  quickCheck (semigroupAssoc :: TrivAssoc)
  quickCheck (semigroupAssoc :: IdenAssoc)
  quickCheck (semigroupAssoc :: TwoAssoc)
  quickCheck (semigroupAssoc :: ThreeAssoc)
  quickCheck (mli :: ThreeMAssoc)
  quickCheck (mri :: ThreeMAssoc)
  quickCheck (semigroupAssoc :: BoolAssoc)
  quickCheck (mli :: BoolConj -> Bool)
  quickCheck (mri :: BoolConj -> Bool)
  quickCheck (semigroupAssoc :: OrAssoc)


newtype Identity a = Identity a
  deriving (Eq, Show)
instance Semigroup a => 
  Semigroup (Identity a) where
    (<>) (Identity a) (Identity b) = 
      Identity (a <> b)
instance Arbitrary a => 
  Arbitrary (Identity a) where
    arbitrary = do
      x <- arbitrary
      return (Identity x)
type IdenAssoc =
  Identity [Int] -> Identity [Int] -> 
  Identity [Int] -> Bool

data Two a b = Two a b
  deriving (Eq, Show)
instance (Semigroup a, Semigroup b) => 
  Semigroup (Two a b) where
    (<>) (Two a b) (Two c d) = 
      Two (a <> c) (b <> d)
instance (Arbitrary a, Arbitrary b) => 
  Arbitrary (Two a b) where
    arbitrary = do
      x <- arbitrary
      y <- arbitrary
      return (Two x y)
type TwoAssoc =
  Two String String -> Two String String -> 
  Two String String -> Bool

data Three a b c = Three a b c
  deriving (Eq, Show)
instance (Semigroup a, Semigroup b, 
  Semigroup c) => Semigroup (Three a b c) 
  where
    (<>) (Three a b c) (Three a' b' c') = 
      Three (a <> a') (b <> b') (c <> c')
instance (Monoid a, Monoid b, Monoid c) => 
  Monoid (Three a b c) where
    mempty = Three mempty mempty mempty 
instance (Arbitrary a, Arbitrary b, 
  Arbitrary c) => Arbitrary (Three a b c)
  where
    arbitrary = do
      x <- arbitrary
      y <- arbitrary
      z <- arbitrary
      return $ Three x y z
type ThreeAssoc = Three String String
  String ->
  Three String String
  String ->
  Three String String
  String -> Bool
type ThreeMAssoc = Three [String] 
  (Maybe (Product Float))
  [Either (Sum Double) Char] -> Bool

newtype BoolConj = BoolConj Bool
  deriving (Eq, Show)
instance Semigroup BoolConj where
  (<>) (BoolConj True) (BoolConj True) = 
    BoolConj True
  (<>) _ _ = BoolConj False
instance Monoid BoolConj where
  mempty = BoolConj True
  mappend = (<>)
instance Arbitrary BoolConj where
  -- arbitrary = oneof [return $ BoolConj True,
  --   return $ BoolConj False]
  arbitrary = do
    x <- arbitrary :: Gen Bool
    return $ BoolConj x
type BoolAssoc = BoolConj ->
  BoolConj -> BoolConj -> Bool

data Or a b =
  Fst a | Snd b
  deriving (Eq, Show)
instance (Semigroup a, Semigroup b) =>
  Semigroup (Or a b) where
    (<>) (Fst _) (Fst b) = Fst b
    (<>) (Fst _) (Snd b) = Snd b
    (<>) (Snd a) (Fst _) = Snd a
    (<>) (Snd a) (Snd _) = Snd a
instance (Arbitrary a, Arbitrary b) =>
  Arbitrary (Or a b) where
    arbitrary = do
      x <- arbitrary
      y <- arbitrary
      oneof [return $ Fst x, 
        return $ Snd y]
type OrAssoc = Or (Sum Int) (Sum Int) -> 
  Or (Sum Int) (Sum Int) -> 
  Or (Sum Int) (Sum Int) -> Bool



newtype Combine a b =
  Combine { unCombine :: a -> b }
  -- deriving (Eq, Show)
instance Show b =>
  Show (Combine a b) where
    show _ = "Combine"
instance Semigroup b =>
  Semigroup (Combine a b) where
    (<>) (Combine b1) (Combine b2) = 
      Combine (b1 <> b2)
f :: Combine Int (Sum Int)
f = Combine $ \n -> Sum (n + 1)
g :: Combine Int (Sum Int)
g = Combine $ \n -> Sum (n - 1)
test :: Sum Int
test = unCombine (f <> g) 0
test2 :: Sum Int
test2 = unCombine f 0
-- -- this doesn't work:
-- -- No instance for (Semigroup Int) 
-- -- arising from a use of ‘<>’
-- h :: Combine Int Int
-- h = Combine $ \n -> n + 1
-- test3 :: Int
-- test3 = unCombine (h <> h) 0
-- -- but this works, Semigroup 
-- -- not involved:
-- test3 :: Int
-- test3 = unCombine h 0
-- instance (CoArbitrary a, Arbitrary b) => 
--   Arbitrary (Combine a b) where
--     arbitrary = do
--       fmap Combine arbitrary


-- -- this failed:
-- stimes' :: (Semigroup a, Integral b) => 
--   b -> (t -> a) -> t -> a
-- stimes' n f2 e = stimes n (f2 e)
-- f2 :: Sum Int -> Sum Int
-- f2 = stimes' 3 (*2)
-- test4 :: Sum Int
-- test4 = f2 (5 :: Sum Int)



