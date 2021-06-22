{-# LANGUAGE TypeApplications #-}

test1 = elem @[] @Char 'c' "abc"
test2 = elem 'c' ['a'..'c']

pair :: a -> [b] -> (a,[b])
pair x y = (x,y)
test = pair @Int @Char 1 "a"


