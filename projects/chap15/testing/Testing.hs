module Testing where

import Data.Monoid
import Test.QuickCheck 

monoidAssoc :: (Eq m, Monoid m)
  => m -> m -> m -> Bool
monoidAssoc a b c =
  (a <> (b <> c)) == ((a <> b) <> c)

type S = String
type B = Bool
type MA = S -> S -> S -> B
test :: IO ()
test = quickCheck (monoidAssoc :: MA)
test2 :: IO ()
test2 = verboseCheck (monoidAssoc :: MA)

monoidLeftIdentity :: (Eq m, Monoid m)
  => m -> Bool
monoidLeftIdentity a = (mempty <> a) == a
monoidRightIdentity :: (Eq m, Monoid m)
  => m -> Bool
monoidRightIdentity a = (a <> mempty) == a
mli :: String -> Bool
mli = monoidLeftIdentity
mri :: String -> Bool
mri = monoidRightIdentity
test3 :: IO ()
test3 = quickCheck (mli :: String -> Bool)
test4 :: IO ()
test4 = quickCheck (mri :: String -> Bool)

{-  
type Adjective = String
type Adverb = String
type Noun = String
type Exclamation = String
madlibbin' :: Exclamation
  -> Adverb
  -> Noun
  -> Adjective
  -> String
madlibbin' e adv noun adj =
  e <> "! he said " <>
  adv <> " as he jumped into his car " <>
  noun <> " and drove off with his " <>
  adj <> " wife."
randomChar :: Gen Char
randomChar = elements ['a'..'z']
randomString :: Gen String
randomString = listOf randomChar
genMad :: Gen String
genMad = do 
  e <- randomString
  adv <- randomString
  noun <- randomString
  adj <- randomString
  return (madlibbin' e adv noun adj)
test5 :: IO ()
test5 = sample genMad
-}


data Optional a =
  Nada | Only a
  deriving (Eq, Show)
-- -- this section is optional because 
-- -- Optional a is not Monoidally used
-- instance Semigroup a
--   => Semigroup (Optional a) where
--     (<>) Nada Nada = Nada
--     (<>) Nada (Only y) = Only y
--     (<>) (Only x) Nada = Only x
--     (<>) (Only x) (Only y) = Only (x <> y)
-- instance Monoid a
--   => Monoid (Optional a) where
--     mempty = Nada
--     mappend = (<>)
instance Arbitrary a => 
  Arbitrary (Optional a) where
    arbitrary = do
      x <- arbitrary
      frequency 
        [(1, return Nada), 
        (1, return $ Only x)]

newtype First' a =
  First' { getFirst' :: Optional a }
  deriving (Eq, Show)
instance Semigroup (First' a) where
  (<>) (First' Nada) (First' Nada) = First' Nada
  (<>) (First' Nada) (First' y) = First' y
  (<>) (First' x) (First' Nada) = First' x
  (<>) (First' x) _ = First' x
instance Monoid (First' a) where
  mempty = First' Nada
  mappend = (<>)
firstMappend :: First' a
  -> First' a
  -> First' a
firstMappend = mappend
type FirstMappend =
  First' String
  -> First' String
  -> First' String
  -> Bool
type FstId =
  First' String -> Bool
instance Arbitrary a => 
  Arbitrary (First' a) where
    arbitrary = do
      x <- arbitrary
      frequency 
        [(1, return $ First' Nada), 
        (1, return $ First' x)]
check :: IO ()
check = do
  quickCheck (monoidAssoc :: FirstMappend)
  quickCheck (monoidLeftIdentity :: FstId)
  quickCheck (monoidRightIdentity :: FstId)
onlyOne :: First' Integer
onlyOne = First' (Only 1)
onlyTwo :: First' Integer
onlyTwo = First' (Only 2)
nada :: First' a
nada = First' Nada
test6 :: First' Integer
test6 = onlyOne `mappend` nada
test7 :: First' a
test7 = nada `mappend` nada
test8 :: First' Integer
test8 = onlyOne `mappend` onlyTwo
