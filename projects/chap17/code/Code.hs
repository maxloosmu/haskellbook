-- {-# LANGUAGE FlexibleContexts #-}
-- {-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

module Code where

import Control.Applicative
  ( Applicative(liftA2), ZipList(ZipList) )
import Data.Monoid ( Sum )
import Test.QuickCheck
    ( frequency,
      (==>),
      within,
      quickCheck,
      Arbitrary(arbitrary),
      Property ) 
import Test.QuickCheck.Checkers 
  ( eq, quickBatch, EqProp(..) )
import Test.QuickCheck.Classes 
  ( applicative, functor, monoid ) 
import Test.QuickCheck.Arbitrary ()

-- 17.8 ZipList Monoid
----------------------
-- this is for testing if as == [] is a 
-- problem, and it is a problem
mconcatP :: forall a. 
  (Eq a, EqProp a, Monoid a) => 
  [a] -> Property
mconcatP as = within 1000 $ mconcat as =-= 
  foldr mappend mempty as
mconcatP' :: (Eq a, Monoid a) => 
  [a] -> Property
mconcatP' as = as /= []  ==> 
  mconcat as == foldr mappend mempty as

zl :: ZipList (Sum Int)
zl = ZipList [1,1 :: Sum Int]
instance Semigroup a 
  => Semigroup (ZipList a) where
    (<>) = liftA2 (<>)
instance (Eq a, Monoid a)
  => Monoid (ZipList a) where
    mempty = pure mempty 
    mappend = (<>)
    mconcat as = 
      foldr mappend mempty as
main :: IO ()
main = do 
  quickBatch $ monoid zl
  -- quickCheck (mconcatP @(ZipList (Sum Int)))
  -- quickCheck (mconcatP' @(ZipList (Sum Int)))
