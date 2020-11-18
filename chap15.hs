{-  -}
{-# LANGUAGE GADTSyntax, TypeApplications, ScopedTypeVariables, ExplicitForAll #-}

import Data.Monoid
import GHC.Types
import Data.List.NonEmpty as N
import Data.Semigroup as S

{-  
test :: Integer
test = getProduct $ Product 1 <> 
  (Product 2 <> Product 3)

xs :: [Sum Integer]
xs = [Sum 1, Sum 2, Sum 3]
test2 :: Sum Integer
test2 = mconcat xs
test3 :: Sum Integer
test3 = foldr mappend mempty xs
test4 :: All
test4 = All True <> All True
test5 :: Any
test5 = Any True <> Any False

x :: Last Integer
x = Last (Just 1)
y :: Last a
y = Last Nothing
test6 :: Last Integer
test6 = x `mappend` y
-}

{-  
data Booly a =
  False'
  | True'
  deriving (Eq, Show)
instance Semigroup (Booly a) where
  (<>) False' _ = False'
  (<>) _ False' = False'
  (<>) True' True' = True'
instance Monoid (Booly a) where
  mempty = True'
x :: Booly a
x = True'
-- -- default: 
-- y :: Booly Any
-- test :: Booly Any
-- y :: Booly a
-- test :: Booly a
y :: Booly GHC.Types.Any
test :: Booly GHC.Types.Any
y = mempty
test = mappend x y
coerce :: a -> Booly a -> Booly a
coerce _ True' = True'
coerce _ False' = False'
a :: Booly Integer
a = coerce 1 True'
b :: Booly Integer
b = coerce 2 False'
ab :: Booly Integer
ab = mappend a b
-}

{-  
data Optional a =
  Nada | Only a
  deriving (Eq, Show)
instance Semigroup a
  => Semigroup (Optional a) where
    (<>) Nada Nada = Nada
    (<>) Nada (Only y) = Only y
    (<>) (Only x) Nada = Only x
    (<>) (Only x) (Only y) = Only (x <> y)
instance Monoid a
  => Monoid (Optional a) where
    mempty = Nada
    mappend = (<>)
onlySum :: Optional (Sum Integer)
onlySum = Only (Sum 1)
test :: Optional (Sum Integer)
test = onlySum `mappend` onlySum
onlyFour :: Optional (Product Integer)
onlyFour = Only (Product 4)
test2 :: Optional (Product Integer)
test2 = onlyFour `mappend` onlyFour
test3 :: Optional (Sum Integer)
test3 = onlySum `mappend` Nada
test4 :: Optional (Product Integer)
test4 = Nada `mappend` onlyFour
test5 :: Optional [Integer]
test5 = Only [1] `mappend` Nada
-}

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
madlibbinBetter' :: Exclamation
  -> Adverb
  -> Noun
  -> Adjective
  -> String
madlibbinBetter' e adv noun adj = 
  mconcat [e, "! he said ", 
  adv, " as he jumped into his car ", 
  noun, " and drove off with his ", 
  adj, " wife."]
test :: String
test = madlibbin' "hey" "quickly" 
  "seat" "excited"
test2 :: String
test2 = madlibbinBetter' "hey" "quickly" 
  "seat" "excited"
-}

{-  
xs :: NonEmpty Int
xs = 1 :| [2, 3]
ys :: NonEmpty Int
ys = 4 :| [5, 6]
test :: NonEmpty Int
test = xs <> ys
test2 :: Int
test2 = N.head xs
test3 :: [Int]
test3 = N.tail test
test4 :: Int
test4 = N.length test
-}

{-  
data Optional a =
  Nada | Only a
  deriving (Eq, Show)
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
onlyOne :: First' Integer
onlyOne = First' (Only 1)
onlyTwo :: First' Integer
onlyTwo = First' (Only 2)
nada :: First' a
nada = First' Nada
-- surprisingly, nada can concretise 
-- to Integer
test6 :: First' Integer
test6 = onlyOne `mappend` nada
test7 :: First' Integer
test7 = nada `mappend` nada
test8 :: First' Integer
test8 = onlyOne `mappend` onlyTwo
-}


instance Semigroup (First' a) where
  (<>) (First' Nada) (First' Nada) = First' Nada
  (<>) (First' Nada) (First' y) = First' y
  (<>) (First' x) (First' Nada) = First' x
  (<>) (First' x) _ = First' x
instance Monoid (First' a) where
  mempty = First' Nada
  mappend = (<>)
data Optional a where
  Nada :: forall a. Optional a
  Only :: forall a. a -> Optional a
  deriving (Eq, Show)
data First' a where
  First' :: forall a. Optional a -> First' a
  deriving (Eq, Show)
-- data Optional a =
--   Nada | Only a
--   deriving (Eq, Show)
-- newtype First' a =
--   First' { getFirst' :: Optional a }
--   deriving (Eq, Show)
-- -- Functions versions of the constructors
nada' :: forall a. Optional a
nada' = Nada @a
onlyOne :: First' Integer
onlyOne = First' @Integer (Only @Integer 1)
onlyTwo :: First' Integer
onlyTwo = First' @Integer (Only @Integer 2)
onlyOneGeneric :: forall a. Num a => First' a
onlyOneGeneric = First' @a (Only @a 1)
-- In more explicit pseudo-haskell:
-- onlyOneGeneric @a @dict_num_a = First' @a 
-- (Only @a (1 @a @dict_num_a))
-- This function takes two implicit parameters, 
-- one for the type and another for the
-- dictionary that describes how the type is 
-- an instance of Num.
nada :: forall a. First' a
nada = First' @a (Nada @a)
test6 :: First' Integer
test6 = mappend @(First' Integer) onlyOne (nada @Integer)
test7 :: forall a. First' a
test7 = mappend @(First' a) (nada @a) (nada @a)
test8 :: First' Integer
test8 = mappend @(First' Integer) onlyOne onlyTwo




