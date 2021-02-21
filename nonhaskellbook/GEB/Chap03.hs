module Chap03 where

import Data.List 
  ( sort, nub )

data DND a b = DND a b
  deriving (Eq, Ord, Show)
data DF a b = DF a b
  deriving (Eq, Ord, Show)
newtype Prime a = Prime a
  deriving (Eq, Ord, Show)

axiom :: Int -> Int -> DND Int Int
axiom x y = DND (x + y) x
rule1 :: DND Int Int -> DND Int Int
rule1 (DND x y) = DND x (x + y)
listAxiom :: [DND Int Int]
listAxiom = [axiom x y | 
  x <- [1..11], y <- [1..11]]
listRule1 :: [DND Int Int]
listRule1 = [rule1 x | 
  x <- listAxiom]
-- -- this also works:
listRule11 :: [DND Int Int]
listRule11 = take 1000 $ concat $ 
  iterate (map rule1) listRule1
fullList :: [DND Int Int]
fullList = (nub . sort) (listAxiom ++ 
  listRule1 ++ listRule11)
-- -- this works:
-- listRule11 :: [DND Int Int]
-- listRule11 = map rule1 listRule1
-- listRule12 :: [DND Int Int]
-- listRule12 = map rule1 listRule11
-- listRule13 :: [DND Int Int]
-- listRule13 = map rule1 listRule12
-- fullList :: [DND Int Int]
-- fullList = sort (listAxiom ++ 
--   listRule1 ++ listRule11 ++
--   listRule12 ++ listRule13)
-- -- this doesn't work:
-- listRule11 :: [DND Int Int]
-- listRule11 
--   | length listRule11 < 10 = 
--     map rule1 (listRule1 ++ 
--     listRule11)
--   | otherwise = listRule11

rule2 :: DND Int Int -> 
  [DF Int Int]
rule2 (DND x y) 
  | x == 2 = [DF y x]
  | otherwise = [] 
listRule2 :: [DF Int Int]
listRule2 = concat [rule2 x | 
  x <- fullList]

rule3 :: DF Int Int -> 
  [DF Int Int]
rule3 (DF z x) = concat (do
  DND a b <- fullList
  if DND (x+1) z == DND a b
    then return [DF z (x+1)]
    else return [])
applyRule3 :: [DF Int Int] -> 
  [DF Int Int]
applyRule3 previousIteration = 
  (nub . sort . concat)
  [ if z > x 
   then rule3 (DF z x) 
   else []
  | (DF z x) <- previousIteration
  ] ++ previousIteration
applyRule3List :: [DF Int Int]
applyRule3List = (nub . sort) $
  take 1000 $ concat $ 
  iterate applyRule3 listRule2
-- -- this works, but too tedious, 
-- -- have to create multiple functions:
-- listRule3 :: [DF Int Int]
-- listRule3 = concat [if z > x 
--   then rule3 (DF z x) 
--   else [] | (DF z x) <- listRule2]
--   ++ listRule2
-- listRule31 :: [DF Int Int]
-- listRule31 = (nub . sort . concat) 
--   [if z > x 
--   then rule3 (DF z x) 
--   else [] | (DF z x) <- listRule3]
--   ++ listRule3

axiom2 :: Prime Int
axiom2 = Prime 2
rule4 :: DF Int Int -> [Prime Int]
rule4 (DF z1 z2) = [Prime z1 | 
  z1 == z2 + 1]
listRule4 :: [Prime Int]
listRule4 = axiom2 : concat [rule4 x | 
  x <- applyRule3List]





