module Logic where

import Data.List

data Form =
  C Bool 
  | V String 
  | Not Form 
  | Form `And` Form 
  | Form `Or` Form
  deriving (Eq, Ord, Show, Read)

test :: Form
test = (V "a" `And` (Not (V "b"))) `Or` 
  ((V "c") `Or` (C False))
test2 :: Form
test2 = Or (And (V "a") (Not (V "b"))) 
  (Or (V "c") (C False))
test3 :: Bool
test3 = ((V "a") `And` (C True)) == V "a"

removeConst :: Form -> Form
removeConst (Not (C True)) = C False
removeConst (Not (C False)) = C True
removeConst (Not x `And` y) = 
  removeConst (removeConst (Not x) `And` y)
removeConst (x `And` (Not y)) = 
  removeConst (x `And` removeConst (Not y))
removeConst (Not x `Or` y) = 
  removeConst (removeConst (Not x) `Or` y)
removeConst (x `Or` (Not y)) = 
  removeConst (x `Or` removeConst (Not y))
removeConst (f `And` (C True)) = f
removeConst ((C True) `And` f) = f
removeConst ((C False) `And` _) = C False
removeConst (_ `And` (C False)) = C False
removeConst (_ `Or` (C True)) = C True
removeConst ((C True) `Or` _) = C True
removeConst ((C False) `Or` f) = f
removeConst (f `Or` (C False)) = f
removeConst f = f
test4 :: Form
test4 = simplifyConst ((V "a") `And` 
  ((C False) `Or` (C True)))
simplifyConst :: Form -> Form
simplifyConst (f1 `And` f2) = 
  removeConst ((simplifyConst f1) `And` 
  (simplifyConst f2))
simplifyConst (f1 `Or` f2) = 
  removeConst ((simplifyConst f1) `Or` 
  (simplifyConst f2))
simplifyConst f = f

test5 :: Form
test5 = nnf (Not ((V "a") `And` 
  ((V "b") `Or` (Not (V "a")))))
test51 :: Form
test51 = nnf (Not (V "a" `Or` Not (V "b")))
nnf :: Form -> Form
nnf (Not (Not f)) = nnf f
nnf (Not (f1 `And` f2)) = 
  (nnf (Not f1)) `Or` (nnf (Not f2))
nnf (Not (f1 `Or` f2)) = 
  (nnf (Not f1)) `And` (nnf (Not f2))
nnf (Not f) = Not (nnf f)
nnf (f1 `And` f2) = (nnf f1) `And` (nnf f2)
nnf (f1 `Or` f2) = (nnf f1) `Or` (nnf f2)
nnf f = f 

test6 :: Form
test6 = cnf ((V "a" `And` V "b") `Or` 
  (V "c" `And` V "d"))
distribOr :: Form -> Form -> Form
distribOr f1 f2 = cnf (f1 `Or` f2)
cnf :: Form -> Form
cnf (f1 `Or` (f2 `And` f3)) = 
  (distribOr f1 f2) `And` 
  (distribOr f1 f3)
cnf ((f1 `And` f2) `Or` f3) = 
  (distribOr f1 f3) `And` 
  (distribOr f2 f3)
cnf f = f

fvList :: Form -> [String]
fvList ((V x) `And` y) = x : fvList y
fvList (x `And` (V y)) = y : fvList x
fvList (Not (V x) `And` y) = x : fvList y
fvList (x `And` Not (V y)) = y : fvList x
fvList ((V x) `Or` y) = x : fvList y
fvList (x `Or` (V y)) = y : fvList x
fvList (Not (V x) `Or` y) = x : fvList y
fvList (x `Or` Not (V y)) = y : fvList x
fvList (Not (V x)) = [x]
fvList (V x) = [x]
fvList x = []
fv :: Form -> [String]
fv = nub . fvList
test7 :: [String]
test7 = fv (V "a" `And` 
  (Not (V "a") `Or` (Not (V "b"))))

test8 :: Form
test8 = subst (V "a" `And` (Not (V "a") 
  `Or` (Not (V "b")))) ("a", True)
subst :: Form -> (String, Bool) -> Form
subst (Not x) (a, b) = 
  Not (subst x (a, b))
subst (x `And` y) (a, b) = 
  subst x (a, b) `And` subst y (a, b)
subst (x `Or` y) (a, b) = 
  subst x (a, b) `Or` subst y (a, b)
subst (V x) (a, b) = 
  if x == a then C b else V x
subst (C x) _ = C x

substAll :: Form -> [(String, Bool)] -> Form
-- substAll = foldl subst
substAll acc [] = acc
substAll acc (y:ys) = 
  substAll (subst acc y) ys
test9 :: Form
test9 = substAll (V "a" `And` 
  (Not (V "a") `Or` (Not (V "b")))) 
  [("a", True), ("b", False)]

test10 :: Bool
test10 = evalSubst (V "a" `And` 
  (Not (V "a") `Or` (Not (V "b")))) 
  [("a", True), ("b", False)]
evalSubst :: Form -> [(String, Bool)] -> Bool
evalSubst acc ys = case simplifyConst 
  (substAll acc ys) of
    C True -> True
    C False -> False
    _ -> error "evalSubst error"

models :: Form -> [(String, Bool)] -> 
  [String] -> [[(String, Bool)]]
models f vl [] = if (evalSubst f vl == True)
  then [vl] else []
models f vl (vn : vns) = 
  models f ((vn, True):vl) vns
  ++ models f ((vn, False):vl) vns
-- allModels must include (nnf f), and 
-- possibly even (cnf (nnf f))
allModels :: Form -> [[(String, Bool)]]
allModels f = models (nnf f) [] (fv (nnf f))
test11 :: [[(String, Bool)]]
test11 = allModels (V "a" `Or` Not (V "b"))
test111 :: [[(String, Bool)]]
test111 = allModels (Not (V "a" `Or` Not (V "b")))

unsatisfiable :: Form -> Bool
unsatisfiable f = allModels f == []
valid :: Form -> Bool
valid f = unsatisfiable (Not f)
test12 :: Bool
test12 = valid (V "a" `Or` (Not (V "b")))
-- test13 is True because allModels
-- (Not (V "a") `And` V "a") is [],
-- so unsatisfiable f is True
test13 :: Bool
test13 = valid (V "a" `Or` (Not (V "a")))
test14 :: Bool
test14 = unsatisfiable (V "a" `Or` (Not (V "b")))
test15 :: Bool
test15 = unsatisfiable (V "a" `Or` (Not (V "a")))

