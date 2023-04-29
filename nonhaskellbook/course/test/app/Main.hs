{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main (main) where

import Lib
import qualified System.Random as R

main2 :: IO ()
main2 = randomWithIO >>= putStrLn

randomWithIO :: IO String
randomWithIO = do
  gen <- R.getStdGen  
  return $ take 5 (R.randomRs ('a','z') gen) 

main :: IO ()
main = runGen <$> R.getStdGen <*> pure random5chars >>= putStrLn

newtype Gen a = MkGen (R.StdGen -> a)
  deriving (Functor)

runGen :: R.StdGen -> Gen a -> a
runGen prng (MkGen g) = g prng

random5chars :: Gen String
random5chars = MkGen $ take 5 . R.randomRs ('a','z')

