import Syntax
import PrintProg

constrVarDecl :: String -> String -> VarDecl ()
constrVarDecl varnm classnm =
  VarDecl () varnm
  (FunT () (ClassT () (ClsNm classnm)) booleanT)
test3 = constrVarDecl "isCar" "Vehicle"
test4 = printVarDecl test3
constrVarDecl2 :: String -> String -> String -> VarDecl ()
constrVarDecl2 varnm classnm1 classnm2 =
  VarDecl () varnm
  (FunT () (ClassT () (ClsNm classnm1))
  (FunT () (ClassT () (ClsNm classnm2)) booleanT))
test5 = constrVarDecl2 "isTaxi" "isCar" "Vehicle"
test6 = printVarDecl test5



