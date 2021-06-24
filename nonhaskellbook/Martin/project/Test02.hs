{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- https://stackoverflow.com/questions/20226902/haskell-kinds-and-type-constraints

class NewConstraint a where
  getValue :: a -> a
newtype Identity a = Identity a
  deriving (Eq, Show)
instance (Num a, NewConstraint a) =>
  NewConstraint (Identity a) where
    getValue x = x
instance NewConstraint Int where
  getValue x = x
test :: Identity Int
test = getValue (Identity (1::Int))

newtype Wrapper = Wrapper Int
  deriving (Show, Num)
test2 :: Wrapper
test2 = Wrapper 1 + Wrapper 1


