{-# LANGUAGE InstanceSigs #-}
module Ex236 where

newtype Moi s a =
  Moi { runMoi :: s -> (a, s) }
  -- deriving Show
instance Functor (Moi s) where
  fmap :: (a -> b) ->
    Moi s a -> Moi s b
  -- fmap f (Moi g) =
  -- using record syntax instead:
  -- runMoi :: Moi s a -> s -> (a, s)
  fmap f (Moi { runMoi = g }) =
    Moi $ \s -> let (a, ns) = g s
                 in (f a, ns)
f0 :: Moi s Int
f0 = (+1) <$> (Moi $ \s -> (0, s))
test :: (Int, Int)
test = runMoi f0 0

