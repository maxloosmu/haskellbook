{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE GADTs #-}
-- {-# LANGUAGE ScopedTypeVariables #-}
data T where
  MkT :: (Show b) => b -> T
pattern ExNumPat :: () => Show b => b -> T
pattern ExNumPat x = MkT x
test11 = ExNumPat "True"
-- testExNumPat :: (forall b. Show b => (b -> T) -> T)
-- testExNumPat (ExNumPat (1::Int)) = MkT 1
-- test12 = testExNumPat (ExNumPat 1)
testExNumPat :: T -> String
testExNumPat (ExNumPat x) = show x
test12 = testExNumPat (ExNumPat 1)
-- output: "1"
f (MkT x) = show x
test13 = f (MkT True)
test14 = f (ExNumPat True)
-- both outputs: "True"

