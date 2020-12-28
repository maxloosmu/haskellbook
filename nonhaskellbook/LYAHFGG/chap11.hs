

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
  fmap f (ZipList1 [x]) = ZipList1 [f x]
  fmap _ (ZipList1 []) = ZipList1 []
  fmap _ (ZipList1 (_:_:_)) = ZipList1 []
instance Applicative ZipList1 where
  pure x = ZipList1 (repeat x)
  ZipList1 fs <*> ZipList1 xs = 
    ZipList1 (zipWith (\f x -> f x) fs xs)
apply3 :: [(Char, Char, Char)]
apply3 = getZipList $ (,,) <$> ZipList1 "dog" 
  <*> ZipList1 "cat" <*> ZipList1 "rat"









