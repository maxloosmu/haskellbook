{-  -}
{-  
import Data.Char 
  (toUpper, isLower, isUpper, ord, chr)
import Control.Monad (forever)
import System.Exit (exitSuccess)
import System.IO 
  (BufferMode(NoBuffering),
  hSetBuffering,
  stdout) 

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  runProgram

runProgram = forever $ do
  putStrLn "\n1. Encode with Caesar Cipher\n\
    \2. Encode with Vigenère Cipher\n\
    \3. End this program\n\
    \Enter 1, 2 or 3 and press enter"
  choice <- getLine
  case choice of
    "1" -> encoding1
    "2" -> encoding2
    "3" -> do
      putStrLn "Goodbye!"
      exitSuccess
    _ -> putStrLn "Choose again.\n"

sentence = "The, quick brown fox, jumps over the lazy dog."
encoding1 :: IO ()
encoding1 = do 
  putStrLn "Sentence before encoding:"
  putStrLn sentence
  putStr "Enter a positive integer: "
  code <- getLine
  if (read code :: Int) >= 0 
  then putStrLn $ encode (read code :: Int) sentence
  else putStrLn "Only positive integer allowed.\n"

encoding2 :: IO ()
encoding2 = do 
  putStrLn "Sentence before encoding:"
  putStrLn sentence
  putStr "Type a word: "
  word <- getLine :: IO String
  if word == ""
  then putStrLn "You did not type a word.\n"
  else putStrLn $ encode2 word sentence

-- previous Hutton's cipher
-- encode 3 "The quick brown fox jumps over the lazy dog."
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
setcode ns xs = take (length (onlyletters xs)) 
  (cycle $ map toUpper ns) 
-- filter out the letters and punctuation
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
test = encode 3 "The quick brown fox, jumps over the lazy dog."
test2 = encode2 "ALLY" "Meet, AT, DAWN"
test3 = csar 3 "The quick brown fox, jumps over the lazy dog."
rotFromTo f t n x = (x - f + n) `mod` (t - f) + f
rotInt n = rotFromTo 32 126 n
rotChar n = chr . rotInt n . ord
csar n = map $ rotChar n
uncs = csar . negate
-}



