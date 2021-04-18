{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns        #-}

-- from:
-- https://kowainik.github.io/posts/haskell-mini-patterns#recursive-go
import qualified Data.Set as Set

sum1 :: Num a => [a] -> a
sum1 [] = 0
sum1 (x:xs) = x + sum1 xs

sumgo :: Num a => [a] -> a
sumgo = go where
  go :: Num a => [a] -> a
  go [] = 0
  go (x:xs) = x + go xs
test1 = sumGo [1,2,3]

sumGo :: forall a . Num a => [a] -> a
sumGo = go 0
  where
    go :: a -> [a] -> a
    go !acc [] = acc
    go acc (x:xs) = go (acc + x) xs

-- -- ordnub suffers from:
-- -- Non-exhaustive patterns in function go
ordnub :: Ord a => [a] -> [a]
ordnub = go 0 0 [] where
  go :: Ord a => Int -> Int -> [a] -> [a] -> [a]
  go n m new (y:ys)
    | new == [] && y:ys == [] = []
    | new == [] && y:ys /= [] =
      go 0 1 (y:new) ys
    | new /= [] && y:ys == [] = new
    | new /= [] && y:ys /= [] && new!!n == y && n<m =
      go 0 m new ys
    | new /= [] && y:ys /= [] && new!!n /= y && n<m =
      go (n+1) m new (y:ys)
    | new /= [] && y:ys /= [] && new!!n /= y && n==m =
      go 0 (m+1) (y:new) ys
test2 = ordNub ([3, 3, 3, 2, 2, -1, 1]::[Int])

ordNub :: forall a . (Ord a) => [a] -> [a]
ordNub = go Set.empty
  where
    go :: Set.Set a -> [a] -> [a]
    go _ []     = []
    go s (x:xs) =
      if x `Set.member` s
      then go s xs
      else x : go (Set.insert x s) xs


