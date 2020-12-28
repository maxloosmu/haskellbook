{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}

-- import GHC.Types
import GHC.Arr

replaceWithP :: b -> Char
replaceWithP = const 'p'
p :: b -> Char
p = replaceWithP

tossEmOne :: Integer -> Integer
tossEmOne = fmap (+1) negate

test :: Bool
test = tossEmOne (-10) == tossEmOne' (-10)
tossEmOne' :: Integer -> Integer
tossEmOne' = (+1) . negate

n :: Maybe a
n = Nothing
w :: Maybe [Char]
w = Just "woohoo"
ave :: Maybe [Char]
ave = Just []
lms :: [Maybe [Char]]
lms = [ave, n, w]
test2 :: Char
test2 = replaceWithP lms
test3 :: [Char]
test3 = fmap replaceWithP lms
test4 :: [Maybe Char]
test4 = (fmap . fmap) replaceWithP lms
test5 :: [Maybe [Char]]
test5 = (fmap . fmap . fmap) 
  replaceWithP lms
-- (.) :: (b -> c) -> (a -> b) 
--   -> a -> c
-- fmap :: Functor f1 => (b -> c) 
--   -> f1 b -> f1 c
-- fmap :: Functor f2 => (a -> b) 
--   -> f2 a -> f2 b
-- (fmap . fmap) :: (Functor f1, Functor f2) 
--   => (a -> b) -> f1 (f2 a) -> f1 (f2 b)
-- (fmap .) :: Functor f => (a1 -> a2 -> b) 
--   -> a1 -> f a2 -> f b
-- (. fmap) :: Functor f => ((f a -> f b) -> c) 
--   -> (a -> b) -> c
-- type check by first applying Functor f2
-- which is (a -> b) 
-- after which apply Functor f1 which is
-- (b -> c)
-- equivalent to (.) :: (b -> c) -> (a -> b) 
-- -> a -> b -> c where b is extra term to 
-- illustrate
ha :: Maybe [[Char]]
ha = Just ["Ha", "Ha"]
lmls :: [Maybe [[Char]]]
lmls = [ha, Nothing, Just []]
-- lms and lmls are different, cannot mix
-- terms because terms in list are consistent
test6 :: [Maybe [[Char]]]
test6 = (fmap . fmap . fmap . fmap) 
  replaceWithP lmls
liftedReplace :: Functor f => f a -> f Char
liftedReplace = fmap replaceWithP
liftedReplace' :: [Maybe [Char]] -> [] Char
liftedReplace' = liftedReplace
twiceLifted :: (Functor f1, Functor f) =>
  f (f1 a) -> f (f1 Char)
twiceLifted = (fmap . fmap) replaceWithP
twiceLifted' :: [Maybe [Char]]
  -> [] (Maybe Char)
twiceLifted' = twiceLifted
thriceLifted ::
  (Functor f2, Functor f1, Functor f)
  => f (f1 (f2 a)) -> f (f1 (f2 Char))
thriceLifted =
  (fmap . fmap . fmap) replaceWithP
thriceLifted' :: [Maybe [Char]]
  -> [] (Maybe ([] Char))
thriceLifted' = thriceLifted

main :: IO ()
main = do
  putStr "replaceWithP lms: "
  print (replaceWithP lms)
  putStr "liftedReplace lms: "
  print (liftedReplace lms)
  putStr "liftedReplace' lms: "
  print (liftedReplace' lms)
  putStr "twiceLifted lms: "
  print (twiceLifted lms)
  putStr "twiceLifted' lms: "
  print (twiceLifted' lms)
  putStr "thriceLifted lms: "
  print (thriceLifted lms)
  putStr "thriceLifted' lms: "
  print (thriceLifted' lms)

a :: [Int]
a = fmap (+1) $ read "[1]" :: [Int]
b :: Maybe [[Char]]
b = (fmap . fmap) (++ "lol") 
  (Just ["Hi,", "Hello"])
-- for functions, fmap lift the operators
-- for c, '-' is lifted
c :: Integer -> Integer
c = (*2) <$> (\x -> x - 2)
-- ** WATCHH ** I/O Monad, return, readIO
d :: Integer -> [Char]
d = fmap ((return '1' ++) . show) 
  (\x -> [x, 1..3])
test7 :: [Char]
test7 = return '1' ++ show [1..3 :: Integer]
test71 :: [Char]
test71 = ((return '1' ++) . show) 
  [1..3 :: Integer]
test72 :: [Char]
test72 = "1" ++ show [1..3 :: Integer]
test73 :: [Char]
test73 = return '1' ++ "23"
test74 :: [Char]
test74 = return '1' :: [Char]
test75 :: [Char]
test75 = (("1" ++) . show) [1..3 :: Integer]
test76 :: [[Char]]
test76 = fmap (("1" ++) . show) 
  [1..3 :: Integer]
test77 :: Integer -> [Char]
test77 = fmap (("1" ++) . show) 
  (\x -> [x, 1..3])
e :: IO Integer
e = 
  let ioi = readIO "1" :: IO Integer
      changed = read <$> ("123"++) <$> 
        show <$> ioi
  in (*3) <$> changed
ioi2 :: IO Integer
ioi2 = readIO "1" :: IO Integer
test8 :: IO Integer
test8 =  (*3) <$> read <$> ("123"++) <$> 
  show <$> ioi2
-- Type Checking:
-- show <$> ioi :: IO String
-- ("123"++) <$> show <$> ioi :: IO [Char]
-- read <$> ("123"++) <$> show <$> ioi :: 
--   Read b => IO b
-- what's happening here is that IO is lifted
-- by <$> throughout the function, so that 
-- operations can work on the values.  
test9 :: IO String
test9 = show <$> (readIO "1" :: IO Integer)
-- for test10, No instance for IO b, 
-- so use IO Integer.
-- also, read cannot be readIO because 
-- IO a becomes IO (IO a), which is 
-- unreadable
test10 :: IO Integer
test10 = read <$> ("123"++) <$> show <$> ioi2

f :: Integer -> Integer
f = (*2)
g :: Integer -> Integer
g = (+3)
test11 :: (Integer -> c) -> Integer -> c
test11 = (. g) 
test12 :: (a -> Integer) -> a -> Integer
test12 = (f .) 
test13 :: [Maybe Char]
test13 = (fmap . fmap) p lms
test14 :: (a -> b) -> [] a -> [] b
test14 = fmap
test15 :: (a1 -> a2 -> b) -> a1 -> [] a2 -> [] b
test15 = (fmap .)
test16 :: (([] a -> [] b) -> c) -> (a -> b) -> c
test16 = (. fmap)
test17 :: (a -> b) -> [] ([] a) -> [] ([] b)
test17 = (fmap . fmap) 

-- 16.11 Ignoring possibilities
-------------------------------
data Possibly a =
  LolNope | Yeppers a
  deriving (Eq, Show)
instance Functor Possibly where
  fmap _ LolNope = LolNope
  fmap f1 (Yeppers x) = Yeppers (f1 x)

data Sum a b =
  First a | Second b
  deriving (Eq, Show)
instance Functor (Sum a) where
  fmap _ (First x) = First x
  fmap f1 (Second x) = Second (f1 x)
-- First a after fmap, functor not 
-- applied because functor is kind
-- * -> *, so require:
-- instance Functor (Sum a) where,
-- (Sum a) means a is part of (Sum a)
-- structure, so functor is not applied
-- to First a.  Cannot define an instance 
-- on Sum only without the a, 
-- because Sum is kind * -> * -> *,
-- cannot fit functor's kind

-- 16.13 More structure, more functors
--------------------------------------
-- although Wrap (f a) is merged into
-- Wrap fa, fa is still reasoned to be
-- (f a), so must use Functors like 
-- (Just 1), and not (1 :: Integer)
data Wrap f a =
  Wrap (f a)
  deriving (Eq, Show)
instance Functor f
  => Functor (Wrap f) where
    fmap f1 (Wrap fa) = Wrap (fmap f1 fa)
test18 :: Wrap Maybe Integer
test18 = fmap (+1) (Wrap (Just 1))

-- 16.15 What if we want to do something 
-- different?
----------------------------------------
type Nat f g = forall a . f a -> g a
maybeToList :: Nat Maybe []
maybeToList Nothing = []
maybeToList (Just x) = [x]
test19 :: [[String]]
test19 = maybeToList (Just ["great", 
  "good"])

-- if both structure and value are to 
-- change, a must be concretised to Int, 
-- but this is not considered good
type Nat2 f g = f Int -> g Int
degenerateMtl :: Nat2 Maybe []
degenerateMtl Nothing = []
degenerateMtl (Just x) = [x + 1]
test20 :: [Int]
test20 = degenerateMtl (Just 1)

-- 16.16 Functors are unique to a datatype
------------------------------------------
data Tuple a b =
  Tuple a b
  deriving (Eq, Show)
newtype Flip f a b =
  Flip (f b a)
  deriving (Eq, Show)
-- compare Flip (Tuple x y) with 
-- Flip (f b a), so original newtype
-- Flip f a b becomes Flip f y x, 
-- so Flip Tuple String Integer
-- becomes Flip (Tuple Integer String).
-- And instance Functor (Flip Tuple a)
-- requires FlexibleInstances.
-- in this case, Functor is defined to
-- lift 2 structures, Flip and Tuple.
instance Functor (Flip Tuple a) where
  fmap f1 (Flip (Tuple x y)) =
    Flip $ Tuple (f1 x) y
test21 :: Flip Tuple String Integer
test21 = fmap (+1) (Flip (Tuple 1 "blah"))

-- creating Functor without Flip:
data Tuple2 b a =
  Tuple2 a b
  deriving (Eq, Show)
instance Functor (Tuple2 b) where
  fmap f1 (Tuple2 x y) = Tuple2 (f1 x) y
test22 :: Tuple2 String Integer
test22 = fmap (+1) (Tuple2 1 "blah")

-- 16.17 Chapter exercises
--------------------------
-- data Bool is of kind *, so no Functors 
-- possible

data BoolAndSomethingElse a =
  False' a | True' a
  deriving (Eq, Show)
instance Functor BoolAndSomethingElse where
  fmap f1 (False' x) = False' (f1 x)
  fmap f1 (True' x) = True' (f1 x)

data BoolAndMaybeSomethingElse a =
  Falsish | Truish a
instance Functor BoolAndMaybeSomethingElse 
  where
    fmap _ Falsish = Falsish
    fmap f1 (Truish x) = Truish (f1 x)

-- for this, Functor not possible
-- Mu should be * -> *
-- but f is also * -> *
-- so Mu becomes (* -> *) -> *
newtype Mu f = InF { outF :: f (Mu f) }
-- use this to test Mu with VS Code:
-- instance Functor Mu where 

-- D has no type parameter a, so 
-- becomes *, Functor not possible
data D = D (Array Word Word) Int Int
-- use this to test D with VS Code:
-- instance Functor D where
  
data Sum1 b a =
  First1 a | Second1 b
instance Functor (Sum1 e) where
  fmap f1 (First1 a1) = First1 (f1 a1)
  fmap _ (Second1 b1) = Second1 b1

data Company a c b =
  DeepBlue a c | Something b
instance Functor (Company e e') where
  fmap f1 (Something b1) = Something (f1 b1)
  fmap _ (DeepBlue a1 c1) = DeepBlue a1 c1

data More b a =
  L a b a | R b a b
  deriving (Eq, Show)
instance Functor (More x) where
  fmap f1 (L a1 b1 a1') = L (f1 a1) b1 (f1 a1')
  fmap f1 (R b1 a1 b1') = R b1 (f1 a1) b1'
test23 :: More Integer Integer
test23 = fmap (+1) (L 1 2 3)
test24 :: More Integer Integer
test24 = fmap (+1) (R 1 2 3)

data Quant a b =
  Finance | Desk a | Bloor b
instance Functor (Quant a) where
  fmap _ Finance = Finance
  fmap _ (Desk x) = Desk x
  fmap f1 (Bloor x) = Bloor (f1 x)

newtype K a b = K a deriving Show
instance Functor (K a) where
  fmap _ (K x) = K x
-- Flip2 is the same as Flip declared
-- previously
newtype Flip2 f a b =
  Flip2 (f b a)
  deriving (Eq, Show)
instance Functor (Flip2 K a) where
  fmap f1 (Flip2 (K b1)) = Flip2 (K (f1 b1))
-- test25 output: Flip2 (K 2) 
-- this shows that Flip2 (K 1) is the already
-- flipped outcome.  Flip2 f a b has already
-- been flipped to Flip2 (f b a), which is
-- Flip2 (K b1), meaning Flip2 K a Integer
-- has already become Flip2 (K Integer). 
-- this means that Flip2 (K b1) can be used 
-- for Functor adjustments, even though
-- the Functor instance is for (Flip2 K a). 
-- the a has been dropped by the data
-- constructor for Flip2, as it is applied 
-- to (K b1).  
test25 :: Flip2 K a Integer
test25 = fmap (+1) (Flip2 (K 1))

newtype EvilGoateeConst a b =
  GoatyConst b
instance Functor (EvilGoateeConst a) where
  fmap f1 (GoatyConst b1) = GoatyConst (f1 b1)

newtype LiftItOut f a =
  LiftItOut (f a)
instance Functor f => 
  Functor (LiftItOut f) where
    fmap f1 (LiftItOut fa) = 
      LiftItOut (fmap f1 fa)

data Parappa f g a =
  DaWrappa (f a) (g a)
instance (Functor f, Functor g) => 
  Functor (Parappa f g) where
    fmap f1 (DaWrappa fa ga) = 
      DaWrappa (fmap f1 fa) (fmap f1 ga)

data IgnoreOne f g a b =
  IgnoringSomething (f a) (g b)
instance (Functor f, Functor g) => 
  Functor (IgnoreOne f g a) where
    fmap f1 (IgnoringSomething fa gb) = 
      IgnoringSomething fa (fmap f1 gb)

data Notorious g o a t =
  Notorious (g o) (g a) (g t)
instance Functor g => 
  Functor (Notorious g o a) where
    fmap f1 (Notorious go ga gt) = 
      Notorious go ga (fmap f1 gt)

data List a =
  Nil | Cons a (List a)
instance Functor List where 
  fmap _ Nil = Nil
  fmap f1 (Cons x lista) = 
    Cons (f1 x) (fmap f1 lista)

data GoatLord a =
  NoGoat | OneGoat a | 
  MoreGoats (GoatLord a) (GoatLord a)
    (GoatLord a)
instance Functor GoatLord where
  fmap _ NoGoat = NoGoat
  fmap f1 (OneGoat x) = OneGoat (f1 x)
  fmap f1 (MoreGoats ga1 ga2 ga3) = 
    MoreGoats (fmap f1 ga1) (fmap f1 ga2)
      (fmap f1 ga3)

data TalkToMe a =
  Halt | Print String a | 
  Read (String -> a)
instance Functor TalkToMe where
  fmap _ Halt = Halt
  fmap f1 (Print s a1) = Print s (f1 a1)
-- don't need to create extra Functor instance,
-- doesn't work:
-- instance Functor (TalkToMe (Read String)) 
  fmap f1 (Read fsa) = Read (fmap f1 fsa)












