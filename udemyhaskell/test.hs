main :: IO ()
main = putStrLn "Hello from Haskell!"

-- Section 26, 27
data Compass = North | East | South | West
  deriving (Eq, Ord, Enum)
instance Show Compass where
  show North = "North"
  show East = "East"
  show South = "South"
  show West = "West"

data Expression = Number Int
                | Add Expression Expression
                | Subtract Expression Expression
                deriving (Eq, Ord, Show)

-- calculate is recursively called.  
calculate :: Expression -> Int
calculate (Number x) = x
calculate (Add x y) = (calculate x) + (calculate y)
calculate (Subtract x y) = (calculate x) - (calculate y)

newHead :: [a] -> a
newHead [] = error "empty list"
newHead (x:xs) = x

