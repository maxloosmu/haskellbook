module Main where

import Control.Monad (forever) 
import Data.Char (toLower) 
import Data.Maybe (isJust) 
import Data.List (intersperse)
import System.Exit (exitSuccess) 
import System.IO 
  (BufferMode(NoBuffering),
  hSetBuffering,
  stdout) 

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  let puzzle = freshPuzzle (fmap toLower "go")
  runGame puzzle
  
maxNumOfWrongGuesses :: Int
maxNumOfWrongGuesses = 2

data Puzzle =
  Puzzle String [Maybe Char] [Char] [Char]
instance Show Puzzle where
  show (Puzzle answer discovered guessed wrong) =
    answer ++ " " ++ (intersperse ' ' $
    fmap renderPuzzleChar discovered)
    ++ " Guessed so far: " ++ guessed 
    ++ " Wrong guesses: " ++ wrong

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
  go (Puzzle word _ _ _) y = elem y word

alreadyGuessed :: Puzzle -> Char -> Bool
alreadyGuessed x y = go x y where
  go (Puzzle _ _ guessed _) y = elem y guessed

-- wordChar is char from word
-- guessChar is char from filledInSoFar
-- wordChar and guessChar are same length
-- zipper replaces wordChar if matches guessed
fillInCharacter :: Puzzle -> Char -> Puzzle
fillInCharacter (Puzzle word filledInSoFar s wrong) c
  = Puzzle word newFilledInSoFar (c : s) wrong
  where 
    zipper guessed wordChar guessChar =
      if wordChar == guessed
      then Just wordChar
      else guessChar
    newFilledInSoFar =
      zipWith (zipper c)
      word filledInSoFar
    test = wrong
    wrong 
      | elem c word = [] ++ test
      | otherwise = c : test

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
gameOver (Puzzle wordToGuess _ guessed wrong) =
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




