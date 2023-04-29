{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main (main) where

import Lib
import qualified System.Random as R

main :: IO ()
main = runGen prng (MkGen g)

newtype Gen a = MkGen (R.StdGen -> a)
  deriving (Functor)

runGen :: R.StdGen -> Gen a -> a
runGen prng (MkGen g) = g prng