{-  -}
import Data.Monoid
import GHC.Types

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

