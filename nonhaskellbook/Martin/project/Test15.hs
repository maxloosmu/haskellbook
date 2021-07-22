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
printPred xs = [x | x <- xs, x /= '(', x /= ')']
addIs :: [(String, String)] -> [(String, String)]
addIs [] = []
addIs (x:xs) = ("is" ++ fst x, snd x) : addIs xs
constrPredList :: [(String, String)] -> [String]
constrPredList xs =
  (printPred . printVarDecl) <$>
  (constrVarDeclList . addIs) xs

test1 = [("Car", "Vehicle"),
  ("Truck", "Vehicle"),
  ("Workday", "Day")]
test2 = addIs test1
test3 = constrPredList test1

