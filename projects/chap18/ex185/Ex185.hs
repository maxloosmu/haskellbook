module Ex185 where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes
import Control.Monad

data CountMe a =
  CountMe Integer a
  deriving (Eq, Show)
instance Functor CountMe where
  fmap f (CountMe i a) =
    CountMe i (f a)
instance Applicative CountMe where
  pure = CountMe 0
  CountMe n f <*> CountMe n' a =
    CountMe (n + n') (f a)
instance Monad CountMe where
  return = pure
  CountMe n a >>= f =
    let CountMe n' b = f a
    in CountMe (n + n') b

instance Arbitrary a
  => Arbitrary (CountMe a) where
  arbitrary =
    CountMe <$> arbitrary <*> arbitrary
instance Eq a => EqProp (CountMe a) where
  (=-=) = eq
main = do
  let trigger :: CountMe (Int, String, Int)
      trigger = undefined
  quickBatch $ functor trigger
  quickBatch $ applicative trigger
  quickBatch $ monad trigger

test = fmap ((+1) . (+2)) [1..5]
test0 = fmap (+1) . fmap (+2) $ [1..5]

mcomp :: (t1 -> t2) -> (t3 -> t1) -> t3 -> t2
mcomp f g a = f (g a)
test1 = mcomp (+(1::Int)) (+(2::Int)) (1::Int)
mcomp0 :: Monad m => (b -> m c)
  -> (a -> m b) -> a -> m c
mcomp0 f g a = join (f <$> (g a))
test2 = mcomp0 (Just) (Just) (1::Int)
test3 = mcomp0 (:[]) (:[]) (1::Int)
test4 = mcomp0 Right Left (1::Int)
test5 = mcomp0 (Just) (\x -> Just (x+1)) (1::Int)
mcomp1 f g a = g a >>= f
test6 = mcomp1 (Just) (Just) (1::Int)
mcomp2 f g a = f =<< g a
test7 = mcomp2 (Just) (Just) (1::Int)

