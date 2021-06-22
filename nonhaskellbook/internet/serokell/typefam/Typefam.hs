{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
-- {-# LANGUAGE DeriveGeneric #-}
-- {-# LANGUAGE DefaultSignatures #-}
-- {-# LANGUAGE StandaloneDeriving #-}

-- https://serokell.io/blog/type-families-haskell
module Typefam where

-- Module ‘Data.Singletons’ does not export ‘Type’
-- import Data.Singletons (Type)
import Data.Kind (Type)

-- :set -XDataKinds
-- :kind! Append [1, 2, 3] [4, 5, 6]
type Append :: forall a. [a] -> [a] -> [a]  -- kind signature
type family Append xs ys where              -- header
  Append '[]    ys = ys                     -- clause 1
  Append (x:xs) ys = x : Append xs ys       -- clause 2

-- :kind! Not 'False
type Not :: Bool -> Bool
type family Not a where
  Not 'True = 'False
  Not 'False = 'True

-- :k Bool
-- Bool :: *
-- :kind Bool
-- Bool :: *
-- :kind! Bool
-- Bool :: *
-- = Bool

-- :kind! FromMaybe 1 ('Just 2)
type FromMaybe :: a -> Maybe a -> a
type family FromMaybe d x where
  FromMaybe d 'Nothing = d
  FromMaybe _ ('Just x) = x

-- :kind! Fst '(1,2)
type Fst :: (a, b) -> a
type family Fst t where
  Fst '(x, _) = x

type S :: (Type -> Type) -> Type
data S k = MkS (k Bool) (k Integer)
-- -- these 3 commands cannot Show:
-- MkS (Just True) Nothing :: S Maybe
-- MkS (Left "Hi") (Right 42) :: S (Either String)
newtype Identity a = Identity a
-- MkS (Identity False) (Identity 0) :: S Identity



