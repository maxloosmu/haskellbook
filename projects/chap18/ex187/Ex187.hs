module Ex187 where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes
import Control.Monad
--1---------------
------------------
data Nope a = NopeDotJpg
  deriving (Eq, Show)
instance Functor Nope where
  fmap _ _ = NopeDotJpg
instance Applicative Nope where
  pure _ = NopeDotJpg
  _ <*> _ = NopeDotJpg
instance Monad Nope where
  return = pure
  _ >>= _ = NopeDotJpg
instance (Arbitrary a) =>
  Arbitrary (Nope a) where
    arbitrary = return NopeDotJpg
instance Eq a =>
  EqProp (Nope a) where
    (=-=) = eq
test :: Nope a
test = NopeDotJpg
--2---------------
------------------
data BahEither b a =
  PLeft a | PRight b
  deriving (Eq, Show)
instance (Arbitrary b, Arbitrary a) =>
  Arbitrary (BahEither b a) where
    arbitrary = do
      y <- arbitrary
      x <- arbitrary
      frequency [(1, return $ PRight y),
        (1, return $ PLeft x)]
instance (Eq b, Eq a) =>
  EqProp (BahEither b a) where
    (=-=) = eq
instance Functor (BahEither b) where
  fmap _ (PRight y) = PRight y
  fmap f (PLeft x) = PLeft (f x)
instance Applicative (BahEither b) where
  pure = PLeft
  (PRight y) <*> _ = PRight y
  _ <*> (PRight y) = PRight y
  (PLeft f) <*> (PLeft x) = PLeft (f x)
instance Monad (BahEither b) where
  return = pure
  (PRight y) >>= _ = PRight y
  (PLeft x) >>= k = k x
--3---------------
------------------
newtype Identity a = Identity a
  deriving (Eq, Ord, Show)
instance (Arbitrary a) =>
  Arbitrary (Identity a) where
    arbitrary = do
      Identity <$> arbitrary
instance Eq a =>
  EqProp (Identity a) where
    (=-=) = eq
instance Functor Identity where
  fmap f (Identity x) = Identity (f x)
instance Applicative Identity where
  pure = Identity
  (<*>) (Identity f) (Identity x) =  Identity (f x)
instance Monad Identity where
  return = pure
  (>>=) (Identity x) k = k x
--4---------------
------------------
--refer to Ex178.hs for relevant code
------------------
data List a =
  Nil | Cons a (List a)
  deriving (Eq, Show)
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
instance Monad List where
  return = pure
  Nil >>= _ = Nil
  Cons x xs >>= k = append (k x) (xs >>= k)
instance Arbitrary a =>
  Arbitrary (List a) where
    arbitrary = listGen
-- Cons x <$> listGen works because we're
-- lifting the Gen in listGen
listGen :: Arbitrary a => Gen (List a)
listGen = do
  x <- arbitrary
  frequency [(1, return Nil),
    (10, Cons x <$> listGen)]
instance Eq a => EqProp (List a) where
  (=-=) = eq
------------------
------------------
test0 = j [[1, 2], [], [3]]
test1 = j (Just (Just 1))
test2 = j (Just Nothing)
test3 = join Nothing
-- x >>= id works because:
-- (\x -> (>>=) x) :: Monad m =>
--   m a -> (a -> m b) -> m b
-- id :: a -> a
-- (\x -> (>>=) x id) :: Monad m =>
--   m (m b) -> m b
-- what happens is that (a -> m b)
-- becomes (m a -> m (m b))
-- because a becomes m a
j :: Monad m => m (m a) -> m a
j x = x >>= id
l1 :: Monad m => (a -> b) -> m a -> m b
l1 = fmap
l2 :: Monad m => (a -> b -> c) ->
  m a -> m b -> m c
l2 = liftM2
a :: Monad m => m a -> m (a -> b) -> m b
a = flip ap
-- https://github.com/1Regina/HaskellBook/blob/master/HaskellBookOtherChs/src/Ch18.07MonadInstances.hs
-- https://github.com/johnchandlerburnham/hpfp/blob/master/18/exercises/src/ChapterExercises.hs
meh :: Monad m =>
  [a] -> (a -> m b) -> m [b]
meh [] _ = return []
meh (x:xs) f = (:) <$> f x <*> meh xs f
-- this works because xs is [m a],
-- so (a -> m b) becomes (m a -> m a)
-- due to xs and id,
-- so m [b] becomes m [a]
flipType :: (Monad m) => [m a] -> m [a]
flipType xs = meh xs id



main :: IO ()
main = do
  -- quickBatch $ functor (undefined ::
  --   Nope (Int, Bool, Char))
  -- quickBatch $ applicative (undefined ::
  --   Nope (Int, Bool, Char))
  -- quickBatch $ monad (undefined ::
  --   Nope (Int, Bool, Char))
  -- quickBatch $ functor (undefined ::
  --   BahEither Int (Int, Bool, Char))
  -- quickBatch $ applicative (undefined ::
  --   BahEither Int (Int, Bool, Char))
  -- quickBatch $ monad (undefined ::
  --   BahEither Int (Int, Bool, Char))
  quickBatch $ functor (undefined ::
    Identity (Int, Bool, Char))
  quickBatch $ applicative (undefined ::
    Identity (Int, Bool, Char))
  quickBatch $ monad (undefined ::
    Identity (Int, Bool, Char))
  -- quickBatch $ functor (undefined ::
  --   List (Int, Bool, Char))
  -- quickBatch $ applicative (undefined ::
  --   List (Int, Bool, Char))
  -- quickBatch $ monad (undefined ::
  --   List (Int, Bool, Char))
