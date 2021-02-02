import Data.List ( (\\) )  
-- (\\) is list or set-difference 
-- for unordered lists

primesTo :: (Eq a, Num a, Enum a) => a -> [a]
primesTo m = sieve [2..m] where 
  sieve (x:xs) = x : sieve (xs \\ [x,x+x..m])
  sieve [] = []
check :: [Integer]
check = [2,2+2..11]
check1 :: [Integer]
check1 = [3..11] \\ [2,2+2..11]
divisible :: Integral a => a -> [a]
divisible x = go (primesTo x) where
  go (y:ys) = if rem x y == 0
    then y : go ys
    else go ys
  go [] = []
