import Syntax
import PrintProg

constrVarDecl :: String -> String -> VarDecl ()
constrVarDecl varnm classnm =
  VarDecl () varnm
  (FunT () (ClassT () (ClsNm classnm)) booleanT)
test3 = constrVarDecl "isCar" "Vehicle"
test4 = printVarDecl test3

