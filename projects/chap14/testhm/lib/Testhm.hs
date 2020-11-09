module Testhm where

import Control.Monad (forever) 
import Data.Char (toLower) 
import Data.Maybe (isJust) 
import Data.List (intersperse)
import System.Exit (exitSuccess) 
import System.IO 
  (BufferMode(NoBuffering),
  hSetBuffering,
  stdout) 
import System.Random (randomRIO) 

start :: IO ()
start = do
  hSetBuffering stdout NoBuffering
  putStrLn "hello world"
  x <- randomWord'
  let puzzle = freshPuzzle (fmap toLower x)
  runGame puzzle

newtype WordList = WordList [String]
allWords :: IO WordList
allWords = do
  dict <- readFile "data/dict.txt"
  return $ WordList (lines dict)

minWordLength :: Int
minWordLength = 2
maxWordLength :: Int
maxWordLength = 2
maxNumOfWrongGuesses :: Int
maxNumOfWrongGuesses = 2
gameWords :: IO WordList
gameWords = do
  WordList aw <- allWords
  return $ WordList (filter gameLength aw) where 
    gameLength w =
      let l = length (w :: String)
      in l >= minWordLength
        && l <= maxWordLength

randomWord :: WordList -> IO String
randomWord (WordList wl) = do
  randomIndex <- randomRIO (0, (length wl) - 1)
  return $ wl !! randomIndex
randomWord' :: IO String
randomWord' = gameWords >>= randomWord

data Puzzle =
  Puzzle String [Maybe Char] [Char] [Char]
  -- deriving Eq
instance Show Puzzle where
  show (Puzzle answer discovered guessed wrong) =
    answer ++ " " ++ intersperse ' ' 
    (fmap renderPuzzleChar discovered)
    ++ " Guessed so far: " ++ guessed 
    ++ " Wrong guesses: " ++ wrong
instance Eq Puzzle where
  Puzzle answer discovered guessed wrong ==
    Puzzle answer2 discovered2 guessed2 wrong2 = True

n :: Maybe a
n = Nothing
renderPuzzleChar :: Maybe Char -> Char
renderPuzzleChar Nothing = '_'
renderPuzzleChar (Just x) = x

freshPuzzle :: String -> Puzzle
freshPuzzle word = Puzzle word (go word) [] [] where
  go [] = []
  go (_:xs) = n : go xs

charInWord :: Puzzle -> Char -> Bool
charInWord x y = go x y where
  go (Puzzle word _ _ _) y1 = elem y1 word

alreadyGuessed :: Puzzle -> Char -> Bool
alreadyGuessed x y = go x y where
  go (Puzzle _ _ guessed _) y1 = elem y1 guessed

-- wordChar is char from word
-- guessChar is char from filledInSoFar
-- wordChar and guessChar are same length
-- zipper replaces wordChar if wordChar match guessed
fillInCharacter :: Puzzle -> Char -> Puzzle
fillInCharacter (Puzzle word filledInSoFar s wrong) c
  | elem c word = 
    Puzzle word newFilledInSoFar (c : s) wrong
  | otherwise = 
    Puzzle word newFilledInSoFar (c : s) (c : wrong)
  where 
    zipper guessed wordChar guessChar =
      if wordChar == guessed
      then Just wordChar
      else guessChar
    newFilledInSoFar =
      zipWith (zipper c)
      word filledInSoFar

handleGuess :: Puzzle -> Char -> IO Puzzle
handleGuess puzzle guess = do
  putStrLn $ "Your guess was: " ++ [guess]
  case (charInWord puzzle guess
    , alreadyGuessed puzzle guess) of
    (_, True) -> do
      putStrLn "You already guessed that\
        \ character, pick \
        \ something else!"
      return puzzle
    (True, _) -> do
      putStrLn "This character was in the\
        \ word, filling in the word\
        \ accordingly"
      return (fillInCharacter puzzle guess)
    (False, _) -> do
      putStrLn "This character wasn't in\
        \ the word, try again."
      return (fillInCharacter puzzle guess)

gameOver :: Puzzle -> IO ()
gameOver (Puzzle wordToGuess _ _ wrong) =
  if (length wrong) > maxNumOfWrongGuesses then
    do 
      putStrLn "Sorry, you've run out of guesses.\
        \  You lose!"
      putStrLn $
        "The word was: " ++ wordToGuess
      exitSuccess
  else return ()
gameWin :: Puzzle -> IO ()
gameWin (Puzzle _ filledInSoFar _ _) =
  if all isJust filledInSoFar then
    do 
      putStrLn "You win!"
      exitSuccess
  else return ()
runGame :: Puzzle -> IO ()
runGame puzzle = forever $ do
  gameOver puzzle
  gameWin puzzle
  putStrLn $
    "Current puzzle is: " ++ show puzzle
  putStr "Guess a letter: "
  guess <- getLine
  case guess of
    [c] -> handleGuess puzzle c >>= runGame
    _ -> putStrLn "Your guess must\
      \ be a single character"




