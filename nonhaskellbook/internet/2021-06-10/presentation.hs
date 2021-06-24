{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
-- {-# LANGUAGE FlexibleInstances #-}

class Something2 a where
  doSomething2 :: a -> Int
  doSomethingWithList :: [a] -> Int
  doSomethingWithList x = 4
instance Something2 Int where
  doSomething2 x = 1
instance Something2 Char where
  doSomething2 x = 2
  doSomethingWithList x = 3
instance Something2 a => Something2 [a] where
  doSomething2 = doSomethingWithList
test = doSomething2 "hello"
test2 = doSomething2 [1,2::Int]

-- -- this doesn't work:
-- class Something2 a where
--   doSomething2 :: a -> Integer
-- instance Something2 Integer where
--   doSomething2 x = 1
-- instance Something2 Char where
--   doSomething2 x = 2
-- instance Something2 String where
--   doSomething2 x = 3
-- instance Something2 [a] where
--   doSomething2 x = 4

-- -- this works with FlexibleInstances
-- class Something a where
--   doSomething :: a -> Integer
-- instance Something Integer where
--   doSomething x = 1
-- instance Something Char where
--   doSomething x = 2
-- instance Something [a] where
--   doSomething x = 3

printing = print $ rank2 (+1)
rank2 :: (forall n. Num n => (n -> n)) -> Double
rank2 f = f 1.0
-- -- This doesn't work:
-- print2 = print $ rank3 (+1) 1
-- rank3 :: ((forall a. a->a) -> Int) -> Bool
-- rank3 f x = f x == 2
-- -- default: rank1 :: Fractional p1 => p2 -> p1
rank1 :: Num n => n -> Double
rank1 x = 1.0

mini :: forall a. Ord a => [a] -> Maybe a
mini [] = Nothing
mini xs = Just $ foldl1 go xs
  where
    go :: a -> a -> a
    go = min
-- mini [1,2]

