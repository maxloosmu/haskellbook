{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}


import Control.Monad (join)

test :: [Integer]
test = fmap (+1) [1..3]
test2 :: [Integer]
test2 = [1..3] >>= return . (+1)
andOne :: Num a => a -> [a]
andOne x = [x, 1]
bind :: Monad m => (a -> m b) -> m a -> m b
bind f1 x = join $ fmap f1 x
bind2 :: Monad m => (a -> m b) -> m a -> m b
bind2 f1 x = f1 =<< x
twoBinds :: IO ()
twoBinds =
  putStrLn "name pls:" >>
    getLine >>=
      \name1 ->
        putStrLn "age pls:" >>
          getLine >>=
            \age1 ->
              putStrLn ("y helo thar: "
              ++ name1 ++ " who is: "
              ++ age1 ++ " years old.")
twiceWhenEven :: [Integer] -> [Integer]
twiceWhenEven xs = do
  x <- xs
  if even x
  then [x*x, x*x]
  else []
test3 :: [Integer]
test3 = twiceWhenEven [1..5]
printOne :: IO ()
printOne = putStrLn "1"
-- printOne, printTwo work because 
-- they're just IO actions
printTwo :: IO ()
printTwo = putStrLn "2"
twoActions :: (IO (), IO ())
twoActions = (printOne, printTwo)
-- -- twoActions won't print
-- -- problem is: IO () is not Showable
-- instance Show (IO ()) where
--   show x = show x
print2Actions :: IO ()
print2Actions = 
  (\(a,b) -> do a; do b) twoActions
print2Actions2 :: IO ()
print2Actions2 = 
  (\(a,b) -> a >> b) twoActions
print2Actions3 :: IO ()
print2Actions3 = 
  uncurry (>>) twoActions
print2Actions4 :: IO ()
print2Actions4 = 
  (fst <> snd) twoActions
print2Actions5 :: IO ()
print2Actions5 = 
  (\(a,b) -> do 
    a
    b) twoActions
separate :: IO ()
separate = fst twoActions

-- 18.4 Examples of Monad use
-- Using the Maybe Monad
-----------------------------
data Cow = Cow {
  name :: String
  , age :: Int
  , weight :: Int
  } deriving (Eq, Show)
noEmpty :: String -> Maybe String
-- even for structure that is Nothing, 
-- >>= will lift the structure and get 
-- the underlying value "ace"
noEmpty "ace" = Nothing
noEmpty str = Just str
noNegative :: Int -> Maybe Int
noNegative n 
  | n >= 0 = Just n
  | otherwise = Nothing
-- if Cow's name is Bess,
-- it must be under 500
weightCheck :: Cow -> Maybe Cow
weightCheck c = 
  if n == "ace" && w > 499
  then Nothing
  else Just c
  where w = weight c
        n = name c
-- cannot use getLine to get cow 
-- name because conflict between
-- IO Cow and Maybe Cow
mkSphericalCow :: String -> Int -> 
  Int -> Maybe Cow
mkSphericalCow name' age' weight' =
  noEmpty name' >>=
  \nammy ->
    noNegative age' >>=
    \agey ->
      noNegative weight' >>=
      \weighty ->
        weightCheck 
        (Cow nammy agey weighty)
test4 :: Maybe Cow
test4 = mkSphericalCow 
  "ace" (1) (500)
f :: Integer -> Maybe Integer
f 0 = Nothing
f n = Just n
g :: Integer -> Maybe Integer
g i =
  if even i
  then Just (i + 1)
  else Nothing
h :: Integer -> Maybe String
h i = Just ("10191" ++ show i)
-- Applicative Maybe need not be added, 
-- same outcome as Monad Maybe and as 
-- doSomething :: Integer -> .. :
-- doSomething :: Applicative Maybe =>
-- Integer -> .. 
doSomething :: Integer -> 
  Maybe (Integer, Integer, String)
doSomething n = do
  a <- f n
  b <- g a
  c <- h b
  pure (a, b, c)
test5 :: Maybe (Integer, Integer, String)
test5 = doSomething 2
f2 :: Maybe String
f2 = Just "2"
g2 :: Maybe Integer
g2 = Just 3
h2 :: Maybe Integer
h2 = Just 4
doSomething1 :: Maybe (String, Integer, Integer)
doSomething1 = do
  a <- f2
  b <- g2
  c <- h2
  pure (a, b, c)




