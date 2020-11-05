module Main where

import Test.Hspec
import WordNumber
  (digit2Word, digits, wordNum)

main :: IO ()
main = hspec $ do
  describe "digit2Word" $ do
    it "returns zero for 0" $ do
      digit2Word 0 `shouldBe` "zero"
    it "returns one for 1" $ do
      digit2Word 1 `shouldBe` "one"
  describe "digits" $ do
    it "returns [1] for 1" $ do
      digits 1 `shouldBe` [1]
    it "returns [1, 0, 0] for 100" $ do
      digits 100 `shouldBe` [1, 0, 0]
  describe "wordNumber" $ do
    it "one-zero-zero given 100" $ do
      wordNum 100
        `shouldBe` "one-zero-zero"
    it "nine-zero-zero-one for 9001" $ do
      wordNum 9001
        `shouldBe` "nine-zero-zero-one"

