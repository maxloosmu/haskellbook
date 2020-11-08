module Testing where

import Test.QuickCheck 
import Test.QuickCheck.Gen (oneof)

data Fool =
  Fulse | Frue
  deriving (Eq, Show)
instance Arbitrary Fool where
  arbitrary = genFool
genFool :: Gen Fool
genFool = do
  oneof [return Fulse, return Frue]
  -- frequency [(2 :: Int, return Fulse), 
  --   (1, return Frue)]
  -- elements [Fulse, Frue]
test :: IO ()
test = sample genFool




