{-  -}
{-  
-- Haskell Programming from first principles
-- Chapter 12 - Signaling Adversity
import Data.List (intersperse)
notThe :: String -> Maybe String
notThe x 
  | x == "the" = Nothing
  | otherwise = Just x
replace :: String -> String
replace = concat . intersperse " " . map seekthe . words
seekthe :: String -> String
seekthe x = if notThe x == Nothing 
  then "a" else x
test = replace "the quick"
-- REDO replace

checkvowel xs = case head xs of
  'a' -> True
  'e' -> True
  'i' -> True
  'o' -> True
  'u' -> True
  _ -> False
test2 = checkvowel "the"
test3 = countTBV2 "the evil cow, the evil cow"
countTBV2 :: String -> Integer
countTBV2 xs = go (words xs) where
  go [x] = 0
  go (x:y:zs) 
    | x == "the" && checkvowel y == True =
      1 + go (y:zs)
    -- | x == "the" && checkvowel y == False =
    --   go (y:zs)
    | otherwise = go (y:zs)
  -- go _ = 0
-- difficult to troubleshoot pattern match:
-- ERROR Non-exhaustive patterns in function go
-- if this statement not included:  
-- go _ the vowel = vowel
-- otherwise, can also include:
-- go [] the vowel = vowel
countTBV xs = go (words xs) 0 0 where
  go [] the vowel = vowel
  go (x:list) the vowel
    -- | [x] == [] = vowel
    | x == "the" = go list 1 vowel
    | x /= "the" && the == 1 && 
      checkvowel x == True = 
      go list 0 (vowel + 1)
    -- | x /= "the" && the == 1 && 
    --   checkvowel x == False = 
    --   go list 0 vowel
    -- | x /= "the" = go list 0 vowel 
    | otherwise = go list 0 vowel
  -- go _ the vowel = vowel

test4 = countvowels "the cow"
isvowel x = elem x "aeiou"
allvowels [] = []
allvowels (x:xs) 
  | isvowel x = x : allvowels xs
  | otherwise = allvowels xs
countvowels :: String -> Integer
countvowels = fromIntegral . length . allvowels
-}

{-  
-- Time taken to create program about 50 min
newtype Word' =
  Word' String
  deriving (Eq, Show)
vowels = "aeiou"
mkWord :: String -> Maybe Word'
mkWord xs = 
  if countingV xs < countingC xs
  then Just (Word' xs)
  else Nothing
isvowel x = 
  if elem x vowels
  then Nothing
  else Just x
counting xs = map isvowel xs
countingV = length . filter (== Nothing) . counting
countingC = length . filter (/= Nothing) . counting
-}

{-  
-- Time taken to create program about 85 min
-- with internet assistance
data Nat =
  Zero
  | Succ Nat
  deriving (Eq, Show)
test = nat2Int (Succ (Succ Zero))
test2 = int2Nat2 2
nat2Int :: Nat -> Integer
nat2Int Zero = 0
nat2Int (Succ xs) = 1 + nat2Int xs
-- recurse on go, not int2Nat2, 
-- to avoid Just (Just (Just ...))
int2Nat2 :: Integer -> Maybe Nat
int2Nat2 x = 
  if x >= 0 
  then Just (go x) 
  else Nothing
  where 
    go 0 = Zero
    go x = Succ (go (x - 1))
-- -- this doesn't work, where cannot take guards
-- -- to do this, expand must be separate function
-- int2Nat x = 
--   if x >= 0 
--   then Just (expand x) 
--   else Nothing
--   where expand x 
--     | x == 0 = Zero
--     | x > 0 = Succ (expand (x - 1))
-}

{-  
-- creation in 10 min, isNothing2
-- copied from internet as alternative
test = isNothing (Just 1)
isJust :: Eq a => Maybe a -> Bool
isJust x =
  if x == Nothing
  then False
  else True
isNothing x =
  if x == Nothing
  then True
  else False
isNothing2 Nothing = True
isNothing2 _ = False

-- creation in 25 min
-- this work only if Num b is added 
-- to type signature:  
mayybee :: Num b => b -> (a -> b) -> Maybe a -> b
mayybee x f Nothing = 0
mayybee x f (Just y) = f y
-- copied from internet as correct alternative
mayybee2 :: b -> (a -> b) -> Maybe a -> b
mayybee2 b f Nothing = b
mayybee2 b f (Just a) = f a
test2 = mayybee 0 (+1) Nothing

-- creation in 5 min
-- similar to previous problem
test3 = fromMaybe 0 Nothing
fromMaybe :: a -> Maybe a -> a
fromMaybe a Nothing = a
fromMaybe a (Just x)= x

-- creation in 10 min
listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing
listToMaybe (x:xs) = Just x
maybeToList :: Maybe a -> [a]
maybeToList Nothing = []
maybeToList (Just x) = [x]
test4 = listToMaybe [1, 2, 3]
test5 = maybeToList (Just 1)


-- created in 15 min, but imperfect
-- requires added (Eq a) to type signature
catMaybes :: Eq a => [Maybe a] -> [a]
catMaybes = map (\(Just x) -> x) . filter (/= Nothing)
test6 = catMaybes6 [Just 1, Nothing, Just 2]
-- this internet idea taken and tried 
-- outside of creation time.
-- in this solution, y variable is in scope.  
catMaybes3 :: [Maybe a] -> [a]
catMaybes3 [] = []
catMaybes3 (x:xs) = 
  case x of 
    Nothing -> catMaybes3 xs
    Just y -> y : catMaybes3 xs
-- -- this solution below doesn't work due to 
-- -- y variable not in scope.  
-- -- Reason:  you can't do a pattern match 
-- -- inside a logical test, unfortunately. 
-- -- Though in other languages like Prolog 
-- -- or Curry you can! 
-- catMaybes2 :: [Maybe a] -> [a]
-- catMaybes2 [] = []
-- catMaybes2 (x:xs)
--   | x == Nothing = catMaybes2 xs
--   | x == (Just y) = y : catMaybes2 xs
-- https://hackage.haskell.org/package/base-4.14.0.0/docs/src/Data.Maybe.html#catMaybes
-- provides a better solution:
catMaybes4 xs = [y | Just y <- xs]

-- contributed by Andreas, solution to 
-- catMaybes2 problem
catMaybes5 :: [Maybe a] -> [a]
catMaybes5 [] = []
catMaybes5 (x:xs)
  | Nothing <- x = catMaybes5 xs
  | (Just y) <- x = y : catMaybes5 xs
catMaybes6 :: [Maybe a] -> [a]
catMaybes6 [] = []
catMaybes6 (Nothing : xs) = catMaybes6 xs
catMaybes6 (Just y  : xs) = y : catMaybes6 xs

-- created in 15 min
flipMaybe :: (Eq a) => [Maybe a] -> Maybe [a]
flipMaybe xs 
  | getnothing xs > 0 = Nothing
  | otherwise = Just [y | Just y <- xs]
  where 
    getnothing = length . filter (== Nothing)
test7 = flipMaybe2 [Just 1, Just 2, Just 3]
test8 = flipMaybe3 [Just 1, Nothing, Just 3]
-- created in 10 min
flipMaybe2 :: (Eq a) => [Maybe a] -> Maybe [a]
flipMaybe2 xs 
  | length [x | x <- xs, x == Nothing] > 0 = Nothing
  | otherwise = Just [y | Just y <- xs]
-- created in 10 min
flipMaybe3 :: [Maybe a] -> Maybe [a]
flipMaybe3 xs 
  | go xs > 0 = Nothing
  | otherwise = Just [y | Just y <- xs]
  where
    go [] = 0
    go (x:xs) =
      case x of
        Nothing -> 1 + go xs
        _ -> go xs
-- -- created in 20 min upon inspiration, but fails
-- -- because cannot have "Just Nothing"
-- -- and cannot have "Just y -> Just (y : go xs)"
-- flipMaybe4 :: [Maybe a] -> Maybe [a]
-- flipMaybe4 (x:xs) = Just (go (x:xs)) where
--   go [] = []
--   go (x:xs) = 
--     case x of 
--       Nothing -> Nothing
--       Just y -> y : go xs
-}

{-  
-- searched internet, read haskellbook 
-- and created in 30 min
s = Left "foo" :: Either String Int
n = Right 3 :: Either String Int
test = either length (*2) s
test2 = either length (*2) n
list = [Left "foo", Right 3, 
  Left "bar", Right 7, Left "baz"]
lefts1 :: [Either a b] -> [a]
lefts1 xs = [x | Left x <- xs]
-- created in 10 min
lefts2 :: [Either a b] -> [a]
lefts2 [] = []
lefts2 (x:xs) = case x of 
  Left y -> y : lefts2 xs
  Right y -> lefts2 xs
-- created in 35 min
lefts3 :: [Either a b] -> [a]
lefts3 = foldr 
  (\a b -> case a of 
    Left y -> y : b
    Right y -> b) []
rights1 :: [Either a b] -> [b]
rights1 = foldr
  (\a b -> case a of
    Right y -> y : b
    Left y -> b) []
rights2 :: [Either a b] -> [b]
rights2 [] = []
rights2 (x:xs) = case x of 
  Right y -> y : rights2 xs
  Left y -> rights2 xs
-- created in 5 min
partEither :: [Either a b] -> ([a], [b])
partEither xs = ([x | Left x <- xs], 
  [x | Right x <- xs])
-- REDO partEither using foldr

-- created in 20 min with internet help
eitherMaybe1 :: (b -> c) 
  -> Either a b -> Maybe c
eitherMaybe1 f (Right x) = Just (f x)
eitherMaybe1 f (Left x) = Nothing
test3 = eitherMaybe2 (1 +) (Right 4)
-- created in 5 min with internet help
-- and inputs from above
either1 :: (a -> c) -> (b -> c)
  -> Either a b -> c
either1 f _ (Left x) = f x
either1 _ g (Right x) = g x
test4 = either1 length (*2) n
-- created in 25 min
eitherMaybe2 :: (b -> c) 
  -> Either a b -> Maybe c
eitherMaybe2 g (Right x) = 
  Just (either1 id g (Right x))
eitherMaybe2 g _ = Nothing
-}


{-  
-- creation in 55 min, 
-- includes reading haskellbook 
-- pages 478-481, and briefly searched 
-- internet for ideas
iter :: (a -> a) -> a -> [a]
iter f x = x : map f (iter f x)
iter2 :: (a -> a) -> a -> [a]
iter2 f x = x : iter2 f (f x)
test = take 10 $ iter (+1) 0


-- after 40 min, need suggested hints
-- created in 65 min
myunfol :: (b -> Maybe (a, b)) -> b -> [a]
myunfol f x = 
  case f x of
    Just (a, b) -> a : myunfol f b
    _ -> []
f1 b = Just (b, b + 1)
test2 = take 10 $ myunfol2 f1 0
myunfol2 f x 
  | Just (a, b) <- f x = a : myunfol2 f b
  | otherwise = []


-- created in 10 min
test3 = take 10 $ betterIter (+1) 0
betterIter :: (a -> a) -> a -> [a]
betterIter f x = myunfol (\x -> Just (x, f x)) x


-- looked for hints after 15 min
-- created in 45 min
data BinaryTree a =
  Leaf
  | Node (BinaryTree a) a (BinaryTree a)
  deriving (Eq, Ord, Show)
unfoldt :: (a -> Maybe (a,b,a))
  -> a -> BinaryTree b
unfoldt f x = go f x 3 where
  go f x stop = if stop > 0
    then case f x of
      Just (a, b, c) -> 
        Node (go f a (stop - 1)) b (go f c (stop - 1)) 
      _ -> Leaf
    else Leaf
f2 a = Just (a + 1, a, a + 1)
test4 = unfoldt f2 0
unfoldt2 :: (a -> Maybe (a,b,a))
  -> a -> BinaryTree b
unfoldt2 f x 
  | Just (a, b, c) <- f x = 
    Node (unfoldt2 f a) b (unfoldt2 f c) 
  | otherwise = Leaf
  
-- look for hints after 15 min
-- created and tested in 65 min
buildtree :: Integer
  -> BinaryTree Integer
buildtree y = unfoldt2 (f y) 0 where
  (f y) x = if y > x 
    then Just (x + 1, x, x + 1)
    else Nothing
-- -- Equational Reasoning based on 
-- -- buildtree's unfoldt2, 
-- -- these cannot be tested on ghci:
-- buildtree 1 
-- = (Node (unfoldt2 (f 1) 1) 0 (unfoldt2 (f 1) 1))
-- = Node Leaf 0 Leaf
-}



-- REDOS
-- created in about 20 min
notThe2 :: String -> Maybe String
notThe2 x = if x == "the"
  then Nothing
  else Just x
replace2 = unwords . go . words where
  go [] = []
  go (x:xs)
    | notThe2 x == Nothing = "a" : go xs
    | otherwise = x : go xs
test = replace2 "the quick"

-- created in about 20 min
-- with hints from internet
-- (bx, by) is the accumulator
partEither2 :: [Either a b] -> ([a], [b])
partEither2 = foldr go ([], []) where
  go a (bx, by)
    | Left x <- a = ((x : bx), by)
    | Right y <- a = (bx, (y : by))
list = [Left "foo"]
list2 = [Left "foo", Right 3, 
  Left "bar", Right 7, Left "baz"]
test2 = partEither2 list2
-- -- Equational Reasoning 
-- -- based on partEither2's go:
-- -- but these cannot be tested on ghci
-- -- except for the last answer:
-- -- created in about 20 min
-- partEither2 = foldr go ([], []) 
--   [Left "foo"]
-- = go (Left "foo") (foldr go ([], []) [])
-- -- substitute bx = [], by = []
-- = (("foo" : []), [])
-- = (["foo"], [])


