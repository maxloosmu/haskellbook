{-  -}
import Data.Monoid

{-  
test :: Integer
test = getProduct $ Product 1 <> 
  (Product 2 <> Product 3)

xs :: [Sum Integer]
xs = [Sum 1, Sum 2, Sum 3]
test2 :: Sum Integer
test2 = mconcat xs
test3 :: Sum Integer
test3 = foldr mappend mempty xs
test4 :: All
test4 = All True <> All True
test5 :: Any
test5 = Any True <> Any False

x :: Last Integer
x = Last (Just 1)
y :: Last a
y = Last Nothing
test6 :: Last Integer
test6 = x `mappend` y
-}


data Booly a =
  False'
  | True'
  deriving (Eq, Show)
instance Semigroup (Booly a) where
  (<>) False' _ = False'
  (<>) _ False' = False'
  (<>) True' True' = True'
instance Monoid (Booly a) where
  mempty = True'
x :: Booly a
x = True'
-- -- default: 
y :: Booly Any
test :: Booly Any
-- y :: Booly a
-- test :: Booly a
y = mempty
test = mappend x y


