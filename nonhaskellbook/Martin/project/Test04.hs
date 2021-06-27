-- test code for stackoverflow discussion

class NewConstraint a where
  getTrue :: a -> a
newtype Identity a = Identity a
  deriving (Show)
instance NewConstraint (Identity a) where
  getTrue x = x
-- test :: Identity Integer
test = getTrue (Identity 1)



