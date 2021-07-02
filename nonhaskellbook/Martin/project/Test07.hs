-- {-# LANGUAGE ExtendedDefaultRules #-}
import Syntax
import Error
data Testing a
  = A {ace :: a, beta :: Testing a}
  | B {ace :: a}
  deriving Show
test = A (1::Int) test2
test2 = B 2

startVar = VarDecl () "" OkT
constrVarDecl :: String -> String -> VarDecl ()
constrVarDecl x y = startVar
  {nameOfVarDecl = x ++ " : " ++ y,
  tpOfVarDecl = booleanT}
test3 = constrVarDecl "isCar" "Vehicle"
test4 = printVarDecl2 test3

printVarDecl2 :: Show t => VarDecl t -> String
printVarDecl2 vd = nameOfVarDecl vd ++ " -> " ++ printTp (tpOfVarDecl vd)




