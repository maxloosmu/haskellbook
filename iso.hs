replaceWithP :: b -> Char
replaceWithP = const 'p'
n :: Maybe a
n = Nothing
w :: Maybe [Char]
w = Just "woohoo"
ave :: Maybe [Char]
ave = Just "Ave"
lms :: [Maybe [Char]]
lms = [ave, n, w]
test4 :: [Maybe Char]
test4 = (fmap . fmap) replaceWithP lms
-- (.) :: (b -> c) -> (a -> b) 
--   -> a -> c
-- fmap :: Functor f1 => (b -> c) 
--   -> f1 b -> f1 c
-- fmap :: Functor f2 => (a -> b) 
--   -> f2 a -> f2 b
-- (fmap . fmap) :: (Functor f1, Functor f2) 
--   => (a -> b) -> f1 (f2 a) -> f1 (f2 b)
-- type check by first applying Functor f2
-- which is (a -> b) 
-- after which apply Functor f1 which is
-- (b -> c)
-- equivalent to (.) :: (b -> c) -> (a -> b) 
--   -> a -> b -> c

