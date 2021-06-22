
import Data.List (sort)

--------------------------
-- Exercises: Eq instances
--------------------------
data TisAnInteger =
  TisAn Integer
  deriving Show
instance Eq TisAnInteger where
  (==) (TisAn x) (TisAn y) = x == y

data TwoIntegers =
  Two Integer Integer
  deriving Show
instance Eq TwoIntegers where
  (==) (Two x1 x2) (Two y1 y2) =
    (x1 == y1) && (x2 == y2)

data StringOrInt =
  TisAnInt Int
  | TisAString String
  deriving Show
instance Eq StringOrInt where
  (==) (TisAnInt x) (TisAnInt y) = x == y
  (==) (TisAString x) (TisAString y) = x == y

data Pair a =
  Pair a a
  deriving Show
instance Eq a => Eq (Pair a) where
  (==) (Pair x1 x2) (Pair y1 y2) =
    (x1 == y1) && (x2 == y2)

data Tuple a b =
  Tuple a b
  deriving Show
instance (Eq a, Eq b) =>
  Eq (Tuple a b) where
    (==) (Tuple x1 x2) (Tuple y1 y2) =
      (x1 == y1) && (x2 == y2)

data Which a =
  ThisOne a
  | ThatOne a
  deriving Show
instance Eq a => Eq (Which a) where
  (==) (ThisOne x) (ThisOne y) = x == y
  (==) (ThatOne x) (ThatOne y) = x == y
  (==) _ _ = False

data EitherOr a b =
  Hello a
  | Goodbye b
  deriving Show
instance (Eq a, Eq b) =>
  Eq (EitherOr a b) where
    (==) (Hello x) (Hello y) = x == y
    (==) (Goodbye x) (Goodbye y) = x == y
    (==) _ _ = False

-- quotRem 10 (-3)
-- (-3,1)
-- divMod 10 (-3)
-- (-4,-2)

-----------------------------
-- Exercises: Will they work?
-----------------------------
-- max (length [1, 2, 3]) (length [8, 9, 10, 11, 12])
-- max [1, 2, 3] [8, 9, 10, 11, 12]
-- max [1, 2, 3] [8, 9]
-- compare (3 * 4) (3 * 5)
-- FAIL: compare "Julie" True
-- (5 + 3) > (3 + 6)

-----------
-- 6.9 Enum
-----------
-- enumFromThenTo 1 10 100
-- enumFromThenTo 0 10 100
-- enumFromThenTo 0 10 99
-- enumFromThenTo 'a' 'c' 'z'

------------
-- 6.10 Show
--------------------
-- Working with Show
--------------------
data Mood = Blah
instance Show Mood where
  show Blah = "Blah"

----------------------------------------
-- 6.12 Instances are dispatched by type
----------------------------------------
class Numberish a where
  fromNumber :: Integer -> a
  toNumber :: a -> Integer
  defaultNumber :: a
newtype Age =
  Age Integer
  deriving (Eq, Show)
instance Numberish Age where
  fromNumber n = Age n
  toNumber (Age n) = n
  defaultNumber = Age 65
newtype Year =
  Year Integer
  deriving (Eq, Show)
instance Numberish Year where
  fromNumber n = Year n
  toNumber (Year n) = n
  defaultNumber = Year 1988
sumNumberish :: Numberish a => a -> a -> a
sumNumberish a a' = fromNumber summed where
  integerOfA = toNumber a
  integerOfAPrime = toNumber a'
  summed =
    integerOfA + integerOfAPrime
test = sumNumberish (Age 10) (Age 10)
-- defaultNumber :: Age
-- defaultNumber :: Year

-------------------------
-- 6.14 Chapter exercises
-------------------------
-- Does it type check?
----------------------
data Person = Person Bool
instance Show Person where
  show (Person True) = "Person True"
  show (Person False) = "Person False"
printPerson :: Person -> IO ()
printPerson person =
  putStrLn (show person)
-- :t printPerson

data Mood0 = Blah0
  | Woot0 deriving (Show, Ord)
instance Eq Mood0 where
  (==) Blah0 Blah0 = True
  (==) Woot0 Woot0 = True
  (==) _ _ = False
settleDown :: Mood0 -> Mood0
settleDown x =
  if x == Woot0
  then Blah0
  else x
-- :t settleDown
-- settleDown Woot0
-- settleDown Blah0
-- settleDown 9
-- Blah0 > Woot0

type Subject = String
type Verb = String
type Object = String
data Sentence =
  Sentence Subject Verb Object
  deriving (Eq, Show)
-- s1 doesn't work:
-- No instance for (Show (Object -> Sentence))
-- arising from a use of ‘print’
-- s1 = Sentence "dogs" "drool"
s2 = Sentence "Julie" "loves" "dogs"

--------------------------------
-- Given a datatype declaration,
-- what can we do?
------------------
data Rocks = Rocks String
  deriving (Eq, Show, Ord)
data Yeah = Yeah Bool
  deriving (Eq, Show, Ord)
data Papu = Papu Rocks Yeah
  deriving (Eq, Show, Ord)
phew = Papu (Rocks "chases")
  (Yeah True)
truth = Papu (Rocks "chomskydoz")
  (Yeah False)
equalityForall :: Papu -> Papu -> Bool
equalityForall p p' = p == p'
comparePapus :: Papu -> Papu -> Bool
comparePapus p p' = p > p'

------------------
-- Match the types
------------------
-- FAIL: i :: a
i :: Num a => a
i = 1

-- FAIL: f :: Num a => a
-- f :: Float
-- f :: Fractional a => a
f :: RealFrac a => a
f = 1.0

-- freud :: a -> a
-- freud :: Ord a => a -> a
freud :: Int -> Int
freud x = x


myX = 1 :: Int
sigmund :: Int -> Int
-- FAIL: sigmund :: a -> a
-- FAIL: sigmund :: Num a => a -> a
sigmund x = myX

-- jung :: Ord a => [a] -> a
-- jung :: [Int] -> Int
jung :: [Char] -> Char
jung xs = head (sort xs)

mySort :: [Char] -> [Char]
mySort = sort
signifier :: [Char] -> Char
-- FAIL: signifier ::
--   Ord a => [a] -> a
signifier xs = head (mySort xs)

--------------------------------------
-- Type-Kwon-Do Two: Electric typealoo
--------------------------------------
chk :: Eq b => (a -> b)
  -> a -> b -> Bool
chk f0 x0 y0 = (f0 x0) == y0

arith :: Num b => (a -> b)
  -> Integer -> a -> b
arith f3 3 x3 = f3 x3 + fromInteger 3
-- FAIL: arith f3 1 x3 = (1 +) x3
