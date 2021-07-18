module Ex233 where
import System.Random
import qualified System.Random.MWC as MWC


sg :: StdGen
sg = mkStdGen 0
-- StdGen {unStdGen = SMGen 0 16294208416658607535}
test :: (Int, StdGen)
test = next sg
-- (-7749304166575005736,StdGen {unStdGen = SMGen 16294208416658607535 16294208416658607535})
newSg :: StdGen
newSg = snd (next sg)
-- StdGen {unStdGen = SMGen 16294208416658607535 16294208416658607535}
test0 :: (Int, StdGen)
test0 = next (snd (next newSg))
-- (-3689361183592386664,StdGen {unStdGen = SMGen 11989137102556719373 16294208416658607535})
test1 :: (Float, StdGen)
test1 = random newSg :: (Float, StdGen)
-- (0.41177148,StdGen {unStdGen = SMGen 14141672759607663454 16294208416658607535})
r :: (Int, StdGen)
r = randomR (0, 3) newSg
-- (0,StdGen {unStdGen = SMGen 14141672759607663454 16294208416658607535})

-- https://hackage.haskell.org/package/mwc-random
-- https://hackage.haskell.org/package/mwc-random-0.14.0.0/docs/System-Random-MWC.html
-- https://discourse.haskell.org/t/fastest-method-to-generate-n-random-outcomes-with-given-distribution/870/4
-- https://alexey.kuleshevi.ch/blog/2019/12/21/random-benchmarks/
main :: IO Word
main = do
  gen <- MWC.createSystemRandom
  MWC.uniformR (0, 10) gen :: IO Word


