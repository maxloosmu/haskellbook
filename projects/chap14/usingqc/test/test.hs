module Main where

import Test.QuickCheck
import Test.Hspec
import Usingqc

main :: IO ()
main = putStrLn "Test suite not yet implemented"

half :: Fractional a => a -> a
half x = x / 2
halfIdentity :: Double -> Double
halfIdentity = (* 2) . half
prop_halfIdentity :: Double -> Bool
prop_halfIdentity x = 
  halfIdentity x == x

-- test.hs cannot be used
runqc3 :: IO ()
runqc3 = quickCheck prop_halfIdentity

