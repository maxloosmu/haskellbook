-- {-# LANGUAGE ExtendedDefaultRules #-}
import Syntax
import PrintProg
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
test4 = printVarDecl test3

