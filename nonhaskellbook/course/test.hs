import qualified System.Random as R

newtype Gen a = MkGen (R.StdGen -> a)
  deriving (Functor)

runGen :: R.StdGen -> Gen a -> a
runGen prng (MkGen g) = g prng