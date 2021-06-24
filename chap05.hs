-- {-# LANGUAGE NoMonomorphismRestriction #-}

celebrate :: [Char] -> [Char]
celebrate = (++ " woot!")
celebrate0 :: [Char] -> [Char]
celebrate0 = ("woot! " ++)
test :: [Char]
test = celebrate "naptime"
test0 :: [Char]
test0 = celebrate0 "dogs"


inlist = (`elem` [1..10])
test1 = inlist 9
-- this doesn't work:
-- inlist0 = (9 `elem`)::(Int -> Bool)
-- test2 = inlist [1..(10::Int)]
hasTen = elem 10
test3 = hasTen [5..15]
----------------------------
-- Exercises: Type arguments
----------------------------
-- must use :t testing1
u = undefined
f :: a -> a -> a -> a
f = u
x :: Char
x = u
testing1 = f x

h :: (Num a, Num b) => a -> b -> b
h = u
testing3 = h 1.0 2
testing4 = h 1 5.5

jackal :: (Ord a, Eq b) => a -> b -> a
jackal = u
testing5 = jackal "keyboard" "has the word jackal in it"
-- testing6 type must be stated due to ambiguous b,
-- type constraint Eq b from jackal is not retained
-- after partial application because GHC tries to
-- resolve Eq b and complains about b.
testing6 :: Eq b => b -> [Char]
testing6 = jackal "keyboard"

kessel :: (Ord a, Num b) => a -> b -> a
kessel = u
testing7 = kessel 1 2
testing8 = kessel 1 (2 :: Integer)
testing9 = kessel (1 :: Integer) 2

jackal0 :: (Ord a, Eq b) => a -> b -> b
jackal0 = u
-- test4 type must be stated, same as testing6
test4 :: Eq b => b -> b
test4 = jackal0 "keyboard"
--------------------------
--------------------------
inc = (+1)
idinc = id inc
test5 = idinc 2
---------------------------
-- Exercises: Parametricity
---------------------------
g1 ::  a -> a -> a
g1 x y = x
g2 ::  a -> a -> a
g2 x y = y
g3 ::  a -> b -> b
g3 x y = y
---------------------
-- 5.6 Type inference
---------------------
-- :t f0 1 2 will result in
-- f0 1 2 :: Num a => a
f0 x y = x + y + 3
----------------------------
-- Exercises: Apply yourself
----------------------------
-- :t myConcat
-- myConcat :: [Char] -> [Char]
myConcat x = x ++ " yo"
-- :t myMult
-- myMult :: Fractional a => a -> a
myMult x = (x / 3) * 5
-- myTake :: Int -> [Char]
myTake x = take x "hey you"
-- myCom :: Int -> Bool
myCom x = x > (length [1..10])
-- myAlph :: Char -> Bool
myAlph x = x < 'z'
------------------------
-- 5.8 Chapter exercises
------------------------
-- Determine the type
---------------------
-- example :: Integer
-- example :: Num p => p
example = 1
-- ex1a :: Integer
-- ex1a :: Num a => a
ex1a = (* 9) 6
-- ex1b :: (Integer, [Char])
-- ex1b :: Num a => (a, [Char])
ex1b = head [(0,"doge"),(1,"kitteh")]
-- ex1c :: (Integer, [Char])
ex1c = head [(0 :: Integer ,"doge"),
  (1,"kitteh")]
-- ex1d :: Bool
ex1d = if False then True else False
-- ex1e :: Int
ex1e = length [1, 2, 3, 4, 5]
-- ex1f :: Bool
ex1f = (length [1, 2, 3, 4]) >
  (length "TACOCAT")
-- w0 :: Integer
-- w0 :: Num a => a
x0 = 5
y0 = x0 + 5
w0 = y0 * 10
-- z0 :: Num a => a -> a
z0 y0 = y0 * 10
-- f1 :: Double
-- f1 :: Fractional a => a
f1 = 4 / y0
x1 = "Julie"
y1 = " <3 "
z1 = "Haskell"
-- f2 :: [Char]
f2 = x1 ++ y1 ++ z1
-------------------
-- Does it compile?
-------------------
bigNum = (^) 5
wahoo = bigNum $ 10

x2 = print
y2 = print "woohoo!"
z2 = x2 "hello world"

a = (+)
b = a 5
c = b 10
d = a c 200

a0 = 12 + b0
b0 = 10000 * a0
----------------------------------------------
-- Type variable or specific type constructor?
----------------------------------------------
-- Num a, Enum b: constrained polymorphic
-- a, b, zed, f, g: fully polymorphic
-- Int, Zed, Blah, C: concrete
type C = Int
f3 :: f3 -> g -> C
f3 = u
-------------------------
-- Write a type signature
-------------------------
-- functionH :: [a] -> a
functionH (x3:_) = x3
-- functionC :: Ord a => a -> a -> Bool
functionC x3 y3 =
  if (x3 > y3) then True else False
-- functionS :: (a, b) -> b
functionS (x3, y3) = y3
-----------------------------------
-- Given a type, write the function
-----------------------------------
myFunc :: (x -> y)
  -> (y -> z)
  -> c
  -> (a, x)
  -> (a, z)
myFunc xToY yToZ _ (a1, x3) =
  (a1, (yToZ (xToY x3)))
i :: a -> a
i x3 = x3
c1 :: a -> b -> a
c1 x3 y3 = x3
c'' :: b -> a -> b
c'' x3 y3 = x3
c' :: a -> b -> b
c' x3 y3 = y3
r :: [a] -> [a]
r = tail
r0 :: [a] -> [a]
r0 = reverse
co :: (b -> c) -> (a -> b) -> a -> c
co g4 f4 x3 = g4 (f4 x3)
a2 :: (a -> c) -> a -> a
a2 f4 x3 = x3
a' :: (a -> b) -> a -> b
a' f4 = f4
---------
-- Fix it
---------
-- module Sing where
fstString :: [Char] -> [Char]
fstString x3 = x3 ++ " in the rain"
sndString :: [Char] -> [Char]
sndString y3 = y3 ++ " over the rainbow"
sing = if x3 > y3 then
  fstString x3 else sndString y3
  where x3 = "Singin"
        y3 = "Somewhere"
-- module Arith3Broken where
main :: IO ()
main = do
  print (1 + 2)
  putStrLn "10"
  print (negate (-1))
  print ((+) 0 blah)
  where blah = negate 1
---------------
-- Type-Kwon-Do
---------------
data Woot
data Blah
f5 :: Woot -> Blah
f5 = undefined
g5 :: (Blah, Woot) -> (Blah, Blah)
g5 (b, w) = (b, f5 w)

f6 :: Int -> String
f6 = undefined
g6 :: String -> Char
g6 = undefined
h6 :: Int -> Char
h6 x3 = g6 (f6 x3)

data A
data B
data C0
q :: A -> B
q = undefined
w :: B -> C0
w = undefined
e :: A -> C0
e a2 = w (q a2)

data X
data Y
data Z
xz :: X -> Z
xz = undefined
yz :: Y -> Z
yz = undefined
xform :: (X, Y) -> (Z, Z)
xform (x3, y3) = (xz x3, yz y3)

munge :: (x -> y)
  -> (y -> (w, z))
  -> x
  -> w
munge f7 g7 x3 = fst (g7 (f7 x3))




