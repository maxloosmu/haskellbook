{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main (main) where

import Lib
import qualified System.Random as R

main :: IO ()
main = randomWithIO >>= putStrLn

newtype Gen a = MkGen (R.StdGen -> a)
  deriving (Functor)

runGen :: R.StdGen -> Gen a -> a
runGen prng (MkGen g) = g prng

randomWithIO :: IO String
randomWithIO = do
  gen <- R.getStdGen  
  return $ take 5 (R.randomRs ('a','z') gen) 
