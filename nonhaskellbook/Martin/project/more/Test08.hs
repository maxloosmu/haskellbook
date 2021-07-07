import Syntax
import Error

startVar = VarDecl () "" OkT
constrVarDecl :: String -> String -> IO ()
constrVarDecl x y = do
  buildDecl y
  buildDecl x
  buildFact1 x
  buildFact2
  buildFact3 x y
  buildRule1 x
  buildRule2 y
  buildRule3 x
  buildRule4 y
buildDecl :: String -> IO ()
buildDecl x = printVarDecl2 $ startVar
  {nameOfVarDecl = "decl is" ++ x ++ " : Class",
  tpOfVarDecl = booleanT}
buildFact1 x = putStrLn $ "fact <" ++ x ++ "ClassIncl>"
buildFact2 = putStrLn "for c: Class"
buildFact3 x y = putStrLn $ "is" ++ x ++
  " c --> is" ++ y ++ " c"
buildRule1 x = putStrLn $ "rule <" ++ x ++ "Incl>"
buildRule2 x = putStrLn $ "for x: " ++ x
buildRule3 x = putStrLn $ "if is" ++ x ++ " x"
buildRule4 x = putStrLn $ "then is" ++ x ++ " x"
test3 = constrVarDecl "Car" "Vehicle"
printVarDecl2 :: Show t => VarDecl t -> IO ()
printVarDecl2 vd = putStrLn $ nameOfVarDecl vd ++
  " -> " ++ printTp (tpOfVarDecl vd)

