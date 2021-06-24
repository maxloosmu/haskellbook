replaceWithP :: b -> Char
replaceWithP = const 'p'
n :: Maybe a
n = Nothing
w :: Maybe [Char]
w = Just "woohoo"
ave :: Maybe [Char]
ave = Just "Ave"
lms :: [Maybe [Char]]
lms = [ave, n, w]
test :: [Maybe Char]
test = (fmap . fmap) replaceWithP lms
-- (.) :: (b -> c) -> (a -> b)
--   -> a -> c
-- fmap :: Functor f1 => (b -> c)
--   -> f1 b -> f1 c
-- fmap :: Functor f2 => (a -> b)
--   -> f2 a -> f2 b
-- (fmap . fmap) :: (Functor f1, Functor f2)
--   => (a -> b) -> f1 (f2 a) -> f1 (f2 b)
-- type check by first applying Functor f2
-- which is (a -> b)
-- after which apply Functor f1 which is
-- (b -> c)
-- equivalent to (.) :: (b -> c) -> (a -> b)
--   -> a -> b -> c

data Form = C Bool | V String
  deriving (Eq, Ord, Show)

test0 :: [Form]
test0 = map V ["a", "b"]

data Rule = Rl String [String]
data Goal = Gl [String]
data Prog = Pr [Rule] Goal

solveProp :: [Rule] -> String -> Bool
solveProp rls p = any (solveGoal rls)
  [ps | Rl h ps <- rls, h == p ]
solveGoal :: [Rule] -> [String] -> Bool
solveGoal rls ps = all (solveProp rls) ps
-- solveGoal  = all . solveProp
runProg :: Prog -> Bool
runProg (Pr rls (Gl ps)) = solveGoal rls ps

human :: Prog
human
  = Pr
  [Rl "h" []]
  (Gl ["h"])
test1 :: Bool
test1 = runProg human
test2 :: Bool
test2 = solveGoal [Rl "h" []] ["h"]
test3 :: Bool
test3 = all (solveProp [Rl "h" []]) ["h"]
test4 :: Bool
test4 = any (solveGoal [Rl "h" []]) [["h"]]
test5 :: [Bool]
test5 = map (solveProp [Rl "h" []]) []



