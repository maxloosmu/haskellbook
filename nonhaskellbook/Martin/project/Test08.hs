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
constrVarDecl :: String -> String -> IO ()
constrVarDecl x y = do
  buildDecl y
  buildDecl x
buildDecl :: String -> IO ()
buildDecl x = printVarDecl2 $ startVar
  {nameOfVarDecl = "decl is" ++ x ++ " : Class",
  tpOfVarDecl = booleanT}
test3 = constrVarDecl "Car" "Vehicle"
printVarDecl2 :: Show t => VarDecl t -> IO ()
printVarDecl2 vd = putStrLn $ nameOfVarDecl vd ++
  " -> " ++ printTp (tpOfVarDecl vd)

