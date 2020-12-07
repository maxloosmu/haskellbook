-- import GHC.Types

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


