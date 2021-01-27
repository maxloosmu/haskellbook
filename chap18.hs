import Control.Monad (join)
import Data.List ( (\\) )  
-- (\\) is list or set-difference 
-- for unordered lists

test :: [Integer]
test = fmap (+1) [1..3]
test2 :: [Integer]
test2 = [1..3] >>= return . (+1)
andOne :: Num a => a -> [a]
andOne x = [x, 1]
bind :: Monad m => (a -> m b) -> m a -> m b
bind f x = join $ fmap f x
bind2 :: Monad m => (a -> m b) -> m a -> m b
bind2 f x = f =<< x
twoBinds :: IO ()
twoBinds =
  putStrLn "name pls:" >>
    getLine >>=
      \name ->
        putStrLn "age pls:" >>
          getLine >>=
            \age ->
              putStrLn ("y helo thar: "
              ++ name ++ " who is: "
              ++ age ++ " years old.")
twiceWhenEven :: [Integer] -> [Integer]
twiceWhenEven xs = do
  x <- xs
  if even x
    then [x*x, x*x]
    else []
test3 :: [Integer]
test3 = twiceWhenEven [1..3]
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









