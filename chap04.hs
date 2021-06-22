

data Mood = Blah | Woot deriving Show
changeMood :: Mood -> Mood
changeMood Blah = Woot
changeMood Woot = Blah

awesome = ["Papuchon", "curry", ":)"]
also = ["Quake", "The Simons"]
allAwesome = [awesome, also]

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome x = x == reverse x

myAbs :: Integer -> Integer
myAbs x = if negate x > 0
  then negate x else x

f :: (a, b) -> (c, d) -> ((b, d), (a, c))
f x y = ((snd x, snd y), (fst x, fst y))

x = (+)
f0 xs = w `x` 1
  where w = length xs
id0 = \x -> x
f1 (a, b) = a


