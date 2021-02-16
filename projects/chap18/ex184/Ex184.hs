module Ex184 where

import Control.Monad (ap)
import Control.Applicative
  ( Applicative(liftA2), ZipList(ZipList) )
-- import Data.Monoid ( Sum )
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
  ( applicative, functor, monoid, monad ) 
import Test.QuickCheck.Arbitrary ()

-----------------------------
-- 18.4 Examples of Monad use
-----------------------------
-- Using the Either Monad
-------------------------
-- years ago
type Founded = Int
-- number of programmers
type Coders = Int

data SoftwareShop =
  Shop { founded :: Founded
    , programmers :: Coders
  } deriving (Eq, Show)
data FoundedError =
  NegativeYears Founded
  | TooManyYears Founded
  | NegativeCoders Coders
  | TooManyCoders Coders
  | TooManyCodersForYears Founded Coders
  deriving (Eq, Show)

validateFounded :: Int
  -> Either FoundedError Founded
validateFounded n
  | n < 0 = Left $ NegativeYears n
  | n > 50 = Left $ TooManyYears n
  | otherwise = Right n
validateCoders :: Int
  -> Either FoundedError Coders
validateCoders n
  | n < 0 = Left $ NegativeCoders n
  | n > 50 = Left $ TooManyCoders n
  | otherwise = Right n
mkSoftware :: Int
  -> Int
  -> Either FoundedError SoftwareShop
mkSoftware years coders = do
  founded2 <- validateFounded years
  programmers2 <- validateCoders coders
  if programmers2 > div founded2 1
  then Left $
    TooManyCodersForYears
    founded2 programmers2
  else Right $ Shop founded2 programmers2
-------------------------------
-- Short Exercise: Either Monad
-------------------------------
data Summ a b =
  First a
  | Second b
  deriving (Eq, Show)
instance Functor (Summ a) where
  fmap _ (First x) = First x
  fmap f (Second y) = Second (f y)
instance Applicative (Summ a) where
  pure = Second
  First x <*> _ = First x 
  Second f <*> y = fmap f y
instance Monad (Summ a) where
  return = pure
  First x >>= _ = First x
  Second y >>= k = k y
instance (Arbitrary a, Arbitrary b) =>
  Arbitrary (Summ a b) where
    arbitrary = do
      x <- arbitrary
      y <- arbitrary
      frequency [(1, return $ First x),
        (1, return $ Second y)]
instance (Eq a, Eq b) =>
  EqProp (Summ a b) where
    (=-=) = eq
    
test :: Summ Int Int
test = Second (+(1::Int)) <*> 
  Second (1::Int)



main :: IO ()
main = do 
  quickBatch $ functor (undefined ::
    Summ Int (Int, Bool, Char))
  quickBatch $ applicative (undefined ::
    Summ Int (Int, Bool, Char))
  quickBatch $ monad (undefined ::
    Summ Int (Int, Bool, Char))
