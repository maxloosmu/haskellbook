-- test code for stackoverflow discussion

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
class NewConstraint a where
  getTrue :: a -> Bool
newtype Identity a = Identity a
  deriving (Eq, Show, Num)
instance NewConstraint (Identity a) where
    getTrue x = True
test :: Bool
test = getTrue (1::Identity Int)



