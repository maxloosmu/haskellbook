module Prolog where

import Logic

data Rule = Rl String [String]
  deriving (Eq, Show)
data Goal = Gl [String]
  deriving (Eq, Show)
data Prog = Pr {rules :: [Rule], goals :: Goal}
  deriving (Eq, Show)

mortalSocrates :: Prog
mortalSocrates = 
  Pr
  [Rl "h" [],
  Rl "m" ["h"]
  ]
  (Gl ["m"])
immortalSocrates :: Prog
immortalSocrates = 
  Pr
  [Rl "h" [],
  Rl "h" ["m"]
  ]
  (Gl ["m"])

implies :: Form -> Form -> Form
implies f1 f2 = Not f1 `Or` f2
conj :: [Form] -> Form
conj = foldl And (C True)
ruleToForm :: Rule -> Form
ruleToForm (Rl x ys) = 
  implies (conj (map V ys)) (V x)
-- data constructors usage similar to 
-- functions, so map is applicable
-- test16 output:
-- [V "a",V "c"]
test16 :: [Form]
test16 = map V ["a", "c"]
goalToForm :: Goal -> Form
goalToForm (Gl xs) = conj (map V xs)
progToForm :: Prog -> Form
progToForm (Pr xs y) = 
  implies (conj $ map ruleToForm xs) 
  (goalToForm y)
-- output of test17 is too complicated
-- for Logic.hs to process:
-- Not ((C True `And` (Not (C True) `Or` V "h")) 
-- `And` (Not (C True `And` V "h") `Or` V "m")) 
-- `Or` (C True `And` V "m")
test17 :: Bool
test17 = valid(progToForm immortalSocrates)
test18 :: Bool
test18 = valid(progToForm mortalSocrates)
test19 :: [[(String, Bool)]]
test19 = allModels (progToForm mortalSocrates)
test20 :: [[(String, Bool)]]
test20 = allModels (Not (progToForm immortalSocrates))

-- Exercises 17 and 18
-- for alldiv3, replace "or" with "and"
anydiv3 :: Integral a => [a] -> Bool
anydiv3 xs = or [x `mod` 3 == 0 | x <- xs]
test21 :: Bool
test21 = anydiv3 [1, 4, 5, 6]
sqneg :: (Ord a, Num a) => [a] -> [a]
sqneg xs = [x * x | x <- xs, x < 0]
test22 :: [Integer]
test22 = sqneg [1, -3, 4, -5]
-- -- this doesn't work:
-- -- Couldn't match expected type ‘[a]’ 
-- -- with actual type ‘Rule’
-- -- In the first argument of ‘head’, 
-- -- namely ‘y’
-- findrules2 :: a -> Prog -> [Rule]
-- findrules2 x ys = 
--   [y | y <- rules ys, x == head y]
-- -- shoudl replace (Rl z zs) with rules,
-- -- else non exhaustive patterns error, 
-- -- this may show that there is recursion
-- -- in pattern matching for list 
-- -- comprehension
findrules :: String -> Prog -> [Rule]
findrules x (Pr rules _) = 
  [Rl y zs | Rl y zs <- rules, y == x]
test23 :: [Rule]
test23 = findrules "d" abcdProg

abcdProg :: Prog
abcdProg
  = Pr
  [Rl "a" [],
  Rl "d" ["b", "c"],
  Rl "d" ["b", "c"],
  Rl "c" ["a"]
  ]
  (Gl ["d", "c"])

-- -- this solves the problem without 
-- -- recursion, gives wrong answer
-- solveProp :: [Rule] -> String -> Bool
-- solveProp rls p = 
--   any (==True) [y == p | Rl y _ <- rls]
-- solveGoal :: [Rule] -> [String] -> Bool
-- solveGoal rls ps = 
--   all (==True) [solveProp rls p | p <- ps]

solveProp2 :: [Rule] -> String -> Bool
solveProp2 rls p = 
  any (solveGoal2 rls) 
  [ps | Rl h ps <- rls, h == p]
solveGoal2 :: [Rule] -> [String] -> Bool
solveGoal2 rls ps = all (solveProp2 rls) ps

runProg :: Prog -> Bool
runProg (Pr rls (Gl ps)) = solveGoal2 rls ps
test24 :: Bool
test24 = runProg abcdProg

nonterminating :: Prog
nonterminating =
  Pr [Rl "h" [],
  Rl "m" ["m", "h"],
  Rl "m" ["h"]
  ]
  (Gl ["m"])
terminating :: Prog
terminating = 
  Pr [Rl "h" [],
  Rl "m" ["h"],
  Rl "m" ["m", "h"]
  ]
  (Gl ["m"])
test25 :: Bool
test25 = runProg nonterminating
test26 :: Bool
test26 = runProg terminating






