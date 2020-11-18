module Testing where

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

-- surprisingly, First' (Only (Only 1))
-- concretise to First' (Optional Integer)
onlyOne :: First' (Optional Integer)
onlyOne = First' (Only (Only 1))
onlyTwo :: First' (Optional Integer)
onlyTwo = First' (Only (Only 2))
nada :: First' a
nada = First' Nada
-- surprisingly, nada can concretise 
-- to Integer
test6 :: First' (Optional Integer)
test6 = onlyOne `mappend` nada
test7 :: First' Integer
test7 = nada `mappend` nada
test8 :: First' (Optional Integer)
test8 = onlyOne `mappend` onlyTwo

newtype First2 a =
  First2 { getFirst2 :: Maybe a }
  deriving (Eq, Show)
instance Semigroup (First2 a) where
  (<>) (First2 Nothing) (First2 Nothing) = 
    First2 Nothing
  (<>) (First2 Nothing) (First2 y) = First2 y
  (<>) (First2 x) (First2 Nothing) = First2 x
  (<>) (First2 x) _ = First2 x
instance Monoid (First2 a) where
  mempty = First2 Nothing
  mappend = (<>)
test9 :: First2 Char
test9 = First2 Nothing `mappend` 
  First2 Nothing




