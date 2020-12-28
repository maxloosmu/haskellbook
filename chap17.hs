
import Data.Char ( toUpper )
import qualified Data.Map as DM
import Data.List (elemIndex)


--17.5 Applicative in use
-------------------------
l :: Maybe String
l = lookup (3::Integer) [(3, "hello")]
l1 :: Maybe Int
l1 = fmap length l
c :: String -> String
c [] = []
c (x:xs) = toUpper x:xs
l2 :: Maybe String
l2 = c <$> l
m :: DM.Map Integer String
m = DM.fromList [(3, "hello")]
m1 :: Maybe String
m1 = fmap c $ DM.lookup 3 m

--Exercises: Lookups
--------------------
added :: Maybe Integer
added = fmap (+3) (lookup (3::Integer) $ 
  zip [1, 2, 3] [4, 5, 6])

y :: Maybe Integer
y = lookup (3:: Integer) $ zip 
  [1, 2, 3] [4, 5, 6]
z :: Maybe Integer
z = lookup (2:: Integer) $ zip 
  [1, 2, 3] [4, 5, 6]
tupled :: Maybe (Integer, Integer)
tupled = (,) <$> y <*> z

x1 :: Maybe Int
x1 = elemIndex (3:: Integer) 
  [1, 2, 3, 4, 5]
y1 :: Maybe Int
y1 = elemIndex (4:: Integer) 
  [1, 2, 3, 4, 5]
max' :: Int -> Int -> Int
max' = max
maxed :: Maybe Int
maxed = max' <$> x1 <*> y1

xs1 :: [Integer]
xs1 = [1, 2, 3]
ys :: [Integer]
ys = [4, 5, 6]
x2 :: Maybe Integer
x2 = lookup (3::Integer) $ zip xs1 ys
y2 :: Maybe Integer
y2 = lookup (2::Integer) $ zip xs1 ys
summed :: Maybe Integer
summed = sum <$> ((,) <$> x2 <*> y2)

newtype Identity a = Identity a
  deriving (Eq, Ord, Show)
instance Functor Identity where
  fmap f (Identity a) = Identity (f a)
instance Applicative Identity where
  pure = Identity
  Identity f <*> Identity a = Identity (f a)
type Id = Identity
xs' :: [Integer]
xs' = [9, 9, 9]
xs2 :: [Integer]
xs2 = const <$> xs1 <*> xs'
-- xs2 output: [1,1,1,2,2,2,3,3,3]
mkId :: a -> Identity a
mkId = Identity
xs3 :: Identity [Integer]
xs3 = const <$> mkId xs1 <*> mkId xs'
-- xs3 output: Identity [1,2,3]
-- Identity is lifted, const keeps
-- [1,2,3] and drops [9,9,9]







