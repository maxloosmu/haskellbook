module Main where

import Test.QuickCheck
import Test.Hspec
import Testhm

guessing :: Gen Char
guessing = elements ['a'..'z']

main :: IO ()
main = hspec $ do
  describe "fillInCharacter" $ do
    it "Puzzle ['w','o','w'] [Nothing, Nothing, \
      \ Nothing] [] [] evaluated gives Puzzle \
      \ ['w','o','w'] [Just 'w', Nothing, Just 'w'] \
      \ ['w'] []" $ do
        fillInCharacter (Puzzle "wow" [Nothing, 
          Nothing, Nothing] [] []) 'w' `shouldBe`
          Puzzle "wow" [Just 'w', Nothing, 
          Just 'w'] "w" []

