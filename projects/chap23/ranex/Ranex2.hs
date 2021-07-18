module Ranex2 where

import Control.Applicative (liftA3)
import Control.Monad (replicateM)
import Control.Monad.Trans.State
import System.Random
import Ranex

-- requires  :set -package transformers
-- and  :l Ranex2.hs
rollDie :: State StdGen Die
rollDie = state $ do
  (n, s) <- randomR (1, 6)
  return (intToDie n, s)
rollDie' :: State StdGen Die
rollDie' =
  intToDie <$> state (randomR (1, 6))
rollDieThreeTimes'
  :: State StdGen (Die, Die, Die)
rollDieThreeTimes' =
  liftA3 (,,) rollDie rollDie rollDie
test :: (Die, Die, Die)
test = evalState rollDieThreeTimes'
  (mkStdGen 1)
-- output: (DieSix,DieFour,DieThree)

infiniteDie :: State StdGen [Die]
infiniteDie = repeat <$> rollDie
gen :: StdGen
gen = mkStdGen 0
test0 :: [Die]
test0 = take 6 $ evalState infiniteDie gen
-- [DieOne,DieOne,DieOne,DieOne,DieOne,DieOne]
nDie :: Int -> State StdGen [Die]
nDie n = replicateM n rollDie
test1 :: [Die]
test1 = evalState (nDie 5) (mkStdGen 0)
-- [DieOne,DieOne,DieOne,DieFour,DieFive]

rollsToGetTwenty :: StdGen -> Int
rollsToGetTwenty g = go 0 0 g where
  go :: Int -> Int -> StdGen -> Int
  go sum2 count gen2
    | sum2 >= 20 = count
    | otherwise =
      let (die, nextGen) =
           randomR (1, 6) gen2
      in go (sum2 + die)
         (count + 1) nextGen
test2 :: Int
test2 = rollsToGetTwenty (mkStdGen 1)
rs :: Int -> Int
rs = (rollsToGetTwenty . mkStdGen)
test3 :: IO Int
test3 = rs <$> randomIO

rollsToGetN :: Int -> StdGen -> Int
rollsToGetN n g = go 0 0 n g where
  go :: Int -> Int -> Int -> StdGen -> Int
  go sum2 count limit gen2
    | sum2 >= limit = count
    | otherwise =
      let (die, nextGen) =
           randomR (1, 6) gen2
      in go (sum2 + die)
         (count + 1) limit nextGen
test4 :: Int
test4 = rollsToGetN 10 (mkStdGen 1)

rollsCountLogged :: Int
  -> StdGen
  -> (Int, [Die])
rollsCountLogged n g = go 0 0 n g [] where
  go :: Int -> Int -> Int -> StdGen
    -> [Die] -> (Int, [Die])
  go sum2 count limit gen2 list
    | sum2 >= limit = (count, list)
    | otherwise =
      let (die, nextGen) =
           randomR (1, 6) gen2
      in go (sum2 + die)
         (count + 1) limit nextGen (list ++ [intToDie die])
test5 :: (Int, [Die])
test5 = rollsCountLogged 10 (mkStdGen 0)
-- (5,[DieOne,DieOne,DieOne,DieFour,DieFive])



