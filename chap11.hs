{-  
-- Haskell Programming from first principles
-- Chapter 11 - Algebraic Datatypes
import Data.Char
-- encode 3 "The quick brown fox jumps over the lazy dog."
-- encode 3 "THE QUICK BROWN FOX JUMPS OVER THE LAZY DOG."
-- decode 3 "Wkh txlfn eurzq ira mxpsv ryhu wkh odcb grj."
let2int :: Char -> Int
let2int c = ord c - ord 'a'
upplet2int c = ord c - ord 'A'
int2let :: Int -> Char
int2let n = chr (ord 'a' + n)
int2upplet n = chr (ord 'A' + n)
shift :: Int -> Char -> Char
shift n c 
  | isLower c = int2let ((let2int c + n) `mod` 26)
  | isUpper c = int2upplet ((upplet2int c + n) `mod` 26)
  | otherwise = c
encode :: Int -> [Char] -> [Char]
encode n xs = [shift n x | x <- xs]
decode n xs = [shift (negate n) x | x <- xs]
-- assume keyword ns is always uppercase
-- assume no punctuation
-- encode2 "ALLY" "MEET AT DAWN"
-- decode2 "ALLY" "MPPR AE OYWY"
-- create keyword through haskell cycle
setcode ns xs = take (length (onlyletters xs)) (cycle ns) 
-- filter out the letters
onlyletters xs = filter (\x -> x /= ' ') xs
-- zip the keyword cycle to the string
applycode [] _ = []
applycode _ [] = []
applycode (n:ns) (x:xs)
  | x /= ' ' = (n,x) : (applycode ns xs)
  | x == ' ' = (' ',x) : (applycode (n:ns) xs)
zipcode ns xs = applycode (setcode ns xs) xs
encode2 ns xs = [shift (upplet2int n) c 
  | (n,c) <- (zipcode ns xs)]
decode2 ns xs = [shift (negate $ upplet2int n) c 
  | (n,c) <- (zipcode ns xs)]
-- csar 3 "The quick brown fox jumps over the lazy dog."
rotFromTo f t n x = (x - f + n) `mod` (t - f) + f
rotInt n = rotFromTo 32 126 n
rotChar n = chr . rotInt n . ord
csar n = map $ rotChar n
uncs = csar . negate
-}


{-  
import Data.List
import Data.Char
test = [issof "blah" "blahwoot",
  issof "blah" "wootblah", 
  issof "blah" "wboloath", 
  issof "blah" "wootbla", 
  issof "blah" "halbwoot", 
  issof "blah" "blawhoot"]
issof [] _ = False
issof _ [] = False
issof xss@(x:xs) yss@(y:ys) 
  | isInfixOf xss yss = True
  | x == y = True && (issof xs ys)
  | x /= y = issof (x:xs) ys
  | [x] /= [] && [y] == [] = False

-- capwords "hello world"
capwords xs = zip (words xs) (words $ uppString xs)
uppString [] = []
uppString (x:xs) = toUpper x : uppString xs
capword [] = []
capword (x:xs) 
  | x /= ' ' = toUpper x : xs
  | otherwise = capword xs
s = "blah. woot ha."
-- cappara s
cappara = concat2 . capfirstword . parameterise '.'
parameterise y [] = []
parameterise y xs = [takeWhile (/= y) xs] ++ 
  parameterise y (dropWhile (== y) 
  (dropWhile (/= y) xs))
capfirstword [] = []
capfirstword (x:xs) = capword x : capfirstword xs
concat2 (x:[]) = x ++ "."
concat2 (x:xs) = (x ++ ". ") ++ concat2 xs
-}


{-  
-- this attempt failed because have not translate
-- from data structure to functions
import Data.List
import Data.Char
data DaPhone = DaPhone String
test = convert $ concat $ 
  keystring ["1","*","2","22","222","**"]
keys x = case x of
  "*" -> "^"
  "**" -> "*"
  "1" -> "1"
  "2" -> "a"
  "22" -> "b"
  "222" -> "c"
  "2222" -> "2"
keystring [] = []
keystring (x:xs) = keys x : keystring xs
capword [] = []
capword (x:xs) = toUpper x : xs
convert [] = []
convert (x:xs) 
  | x == '^' = capword $ convert xs
  | x /= '^' = x : convert xs
-}


{-  
data Expr
  = Lit Integer
  | Add Expr Expr
test = eval (Add (Lit 1) (Lit 9001))
test2 = printExpr (Add (Lit 1) (Lit 9001))
test3 = printExpr a3
-- eval :: Expr -> Integer
eval (Lit x) = x 
eval (Add x y) = eval x + eval y
a1 = Add (Lit 9001) (Lit 1)
a2 = Add a1 (Lit 20001)
a3 = Add (Lit 1) a2
printExpr (Lit x) = show x
printExpr (Add x y) = printExpr x 
  ++ " + " ++ printExpr y
-}


{-
-- :t foldr (\x acc -> show x ++ acc) [] [1,2,3]
afold = foldr rf [] [1, 2, 3]
  where rf x acc = "(" ++ show x ++ ")" ++ acc
-}


import Data.List
import Data.Char
convo :: [String]
convo =
  ["Wanna play 20 questions",
  "Ya",
  "U 1st haha",
  "Lol OK. Have u ever tasted alcohol",
  "Lol ya",
  "Wow ur cool haha. Ur turn",
  "OK. Do u think I am pretty Lol",
  "Lol ya",
  "Just making sure rofl ur turn"]
data DaPhone = DaPhone [String]
  deriving (Eq, Show)
keypad = ["1", "2abc", "3def", "4ghi", 
  "5jkl", "6mno", "7pqrs", "8tuv", "9wxyz", 
  "*", "0+ _", "#.,"]
maxphone = DaPhone keypad
test = reverseTaps maxphone 'c'
test2 = map (cellPhoneDead maxphone) convo
test3 = map fingerTaps test2
test4 = map mostPopLetter convo
test8 = coolestLtr convo
test10 = coolestWord convo
-- validButtons = "1234567890*#"
type Digit = Char 
-- Valid presses: 1 and up
type Presses = Int 
uppercase xs = map (map toUpper) xs
reverseTaps :: DaPhone
  -> Char
  -> [(Digit, Presses)]
reverseTaps (DaPhone ys) x 
  | isUpper x = 
    [('*', 1)] ++ search 
    (DaPhone $ uppercase ys) x
  | otherwise = search (DaPhone ys) x
search (DaPhone []) x = 
  error "Error: Invalid Character"
-- assume ^ character indicates uppercase
-- and is not a valid character
search (DaPhone (y:ys)) x
  | elem x y && x == '*' =
    error "Error: Invalid Character"
  | elem x y && x == head y = 
    [(head y, length y)]
  | elem x y && x /= head y = 
    [(head y, head (elemIndices x y))]
  | otherwise = search (DaPhone ys) x
cellPhoneDead :: DaPhone
  -> String
  -> [(Digit, Presses)]
cellPhoneDead (DaPhone ys) [] = []
cellPhoneDead (DaPhone ys) (x:xs) = 
  reverseTaps (DaPhone ys) x ++ 
  cellPhoneDead (DaPhone ys) xs
fingerTaps :: [(Digit, Presses)] -> Presses
fingerTaps = sum . map snd
maxlength [] = []
maxlength (x:xs)
  | length (maxlength xs) > length x = maxlength xs
  | otherwise = x
letterfreq2 xs = (map (\x -> (length x, head x)) . 
  group . sort) xs
mostPopLetter xs = (snd . maximum . letterfreq2) xs
costofletter = snd . head . (reverseTaps maxphone) . 
  mostPopLetter
totalcostofletter2 xs = (fingerTaps . 
  (cellPhoneDead maxphone) . 
  maxlength . group . sort) xs
coolestLtr xs = (mostPopLetter . concat . 
  onlywords) xs
concat3 [] = []
concat3 (x:xs) = x ++ " " ++ concat3 xs
onlywords = map (takeWhile (/='.')) . words . concat3
wordfreq xs = 
  [(c,x) | x <- onlywords xs,
  let c = (length . filter (== x)) (onlywords xs)]
coolestWord = snd . maximum . wordfreq








