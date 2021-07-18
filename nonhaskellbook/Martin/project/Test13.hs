import Syntax
import PrintProg

constrVarDecl1 :: String -> String -> VarDecl ()
constrVarDecl1 varnm classnm =
  VarDecl () varnm
  (FunT () (ClassT () (ClsNm classnm)) booleanT)
test = constrVarDecl1 "isCar" "Vehicle"
test0 = printVarDecl test
constrVarDecl2 :: String -> String -> String -> VarDecl ()
constrVarDecl2 varnm classnm1 classnm2 =
  VarDecl () varnm
  (FunT () (ClassT () (ClsNm classnm1))
  (FunT () (ClassT () (ClsNm classnm2)) booleanT))
test1 = constrVarDecl2 "isTaxi" "isCar" "Vehicle"
test2 = printVarDecl test1

constrFunT [] = booleanT
constrFunT (classNm:classNms) =
  FunT ()
  (ClassT () (ClsNm classNm))
  (constrFunT classNms)
constrVarDecl ::
  String -> String -> VarDecl ()
constrVarDecl varNm classNmString =
  VarDecl () varNm
  $ constrFunT (words classNmString)
test3 = constrVarDecl "isTaxi" "Car Vehicle"
test4 = printVarDecl test3



