import Syntax
import PrintProg

constrVarDecl :: String -> String -> VarDecl ()
constrVarDecl varnm classnm =
  VarDecl () varnm
  (FunT () (ClassT () (ClsNm classnm)) booleanT)
test = constrVarDecl "isCar" "Vehicle"
test0 = printVarDecl test

constrVarDeclList :: [(String, String)] -> [VarDecl ()]
constrVarDeclList [] = []
constrVarDeclList (classDecl:classDecls) =
  [constrVarDecl (fst classDecl) (snd classDecl)] ++
  constrVarDeclList classDecls

printPred :: String -> String
printPred xs = "is" ++ [x | x <- xs, x /= '(', x /= ')']
constrPredList :: [(String, String)] -> [String]
constrPredList xs =
  (printPred . printVarDecl) <$>
  constrVarDeclList xs
test1 = [("Car", "Vehicle"),
  ("Truck", "Vehicle"),
  ("Workday", "Day")]
test2 = constrPredList test1


