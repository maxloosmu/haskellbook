-- 2nd attempt to resolve Martin's project

{-# LANGUAGE MultiParamTypeClasses #-}
data Superclass = Vehicle | Day | Road
  deriving Show
data Subclass = Car | Truck | Workday | Holiday | Highway
  deriving Show
data Subsubclass = Taxi
  deriving Show
data Class a b = Class a (Extends b)
  deriving Show
newtype Extends b = Extends b
  deriving Show
class Classify a b where
  classify :: (a, b) -> Class a b
instance Classify Subclass Superclass where
  classify (Car, Vehicle) = Class Car (Extends Vehicle)
instance Classify Subsubclass Subclass where
  classify (Taxi, Car) = Class Taxi (Extends Car)

test01 :: Class Subclass Superclass
test01 = Class Car (Extends Vehicle)
test02 :: Class Subclass Superclass
test02 = classify (Car, Vehicle)
test03 :: Class Subsubclass Subclass
test03 = classify (Taxi, Car)


