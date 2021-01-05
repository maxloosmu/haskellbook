-- {-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- {-# LANGUAGE DeriveFunctor #-}


apply :: [t -> Int] -> [t] -> [Int]
apply fs xs = [f x | f <- fs, x <- xs, 
  f x > (50::Int)]
apply1 :: [Int]
apply1 = apply [(*2),(*5),(*10)] [8,10,11]

apply2 :: [Double]
apply2 = (\x y z -> [x,y,z]) <$> 
  (+3) <*> (*2) <*> (/2) $ 5

zipping :: [Double]
zipping = zipWith (**) (replicate 3 2) [1..5] 

newtype ZipList1 a = ZipList1
  { getZipList :: [a] }
  deriving (Eq, Show)
instance Functor ZipList1 where
  fmap f (ZipList1 (x:xs)) = 
    ZipList1 (f x : fmap f xs)
-- -- this works very well:
-- instance Functor ZipList1 where
--   fmap f (ZipList1 xs) = 
--     ZipList1 (fmap f xs)
-- -- this works, but is inflexible with 
-- -- longer strings
-- instance Functor ZipList1 where
--   fmap f (ZipList1 (x1:x2:x3:_)) = 
--     ZipList1 ([f x1]++[f x2]++[f x3])
--   fmap _ (ZipList1 []) = ZipList1 []
instance Applicative ZipList1 where
  pure x = ZipList1 (repeat x)
  ZipList1 fs <*> ZipList1 xs = 
    ZipList1 (zipWith (\f x -> f x) fs xs)
apply3 :: [(Char, Char, Char)]
apply3 = getZipList $ (,,) <$> ZipList1 "doggy" 
  <*> ZipList1 "catty" <*> ZipList1 "ratty"










