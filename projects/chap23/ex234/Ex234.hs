module Ex234 where

type Iso a b = (a -> b, b -> a)
newtype Sum a = Sum { getSum :: a }
  deriving Show
sumIsIsomorphicWithItsContents ::
  Iso a (Sum a)
sumIsIsomorphicWithItsContents =
  (Sum, getSum)
test :: Sum Int
test = (fst sumIsIsomorphicWithItsContents)
  (2::Int)
test0 :: Int
test0 = (snd sumIsIsomorphicWithItsContents)
  Sum {getSum = 2::Int}

