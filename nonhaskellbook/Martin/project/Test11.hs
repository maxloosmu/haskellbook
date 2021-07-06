{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFunctor #-}
import Data.Data (Data, Typeable)

constrVarDecl :: String -> String -> VarDecl ()
constrVarDecl varnm classnm =
  VarDecl () varnm
  (FunT () (ClassT () (ClsNm classnm)) booleanT)
test3 = constrVarDecl "isCar" "Vehicle"
test4 = printVarDecl test3

-- From Syntax.hs
-- import Data.Data (Data, Typeable)
type VarName = String
newtype ClassName = ClsNm {stringOfClassName :: String}
  deriving (Eq, Ord, Show, Read, Data, Typeable)
----- Types
-- TODO: also types have to be annotated with position information
-- for the parser to do the right job
data Tp t
  = ClassT {annotOfTp :: t, classNameOfTp :: ClassName}
  | FunT {annotOfTp :: t, funTp :: Tp t, argTp :: Tp t}
  | TupleT {annotOfTp :: t, componentsOfTpTupleT :: [Tp t]}
  | ErrT {causeOfTpErrT :: t}
  | OkT        -- fake type appearing in constructs (classes, rules etc.) that do not have a genuine type
  | KindT
  deriving (Eq, Ord, Show, Read, Functor, Data, Typeable)
booleanT :: Tp ()
booleanT = ClassT () (ClsNm "Boolean")
integerT :: Tp ()
integerT = ClassT () (ClsNm "Integer")
stringT :: Tp ()
stringT = ClassT () (ClsNm "String")
data VarDecl t = VarDecl {annotOfVarDecl :: t, nameOfVarDecl :: VarName, tpOfVarDecl :: Tp t}
  deriving (Eq, Ord, Show, Read, Functor, Data, Typeable)

-- From PrintProg.hs
printVarDecl :: Show t => VarDecl t -> String
printVarDecl vd = nameOfVarDecl vd ++ ": " ++ printTp (tpOfVarDecl vd)

-- From Error.hs
printClassName :: ClassName -> String
printClassName (ClsNm cn) = cn
printTp :: Tp t -> String
printTp t = case t of
  ClassT _ cn -> printClassName cn
  FunT _ t1 t2 -> "(" ++ printTp t1 ++ " -> " ++ printTp t2 ++")"
  TupleT _ [] -> "()"
  TupleT _ [t] -> "(" ++ printTp t ++ ")"
  TupleT _ (t:ts) -> "(" ++ printTp t ++ ", " ++ (foldr (\s r -> ((printTp s) ++ ", " ++ r)) "" ts) ++ ")"
  _ -> error "internal error in printTp: ErrT or OkT not printable"