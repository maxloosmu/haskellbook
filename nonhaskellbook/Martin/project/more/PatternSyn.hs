-- https://haskell-explained.gitlab.io/blog/posts/2019/08/27/pattern-synonyms/index.html
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}

module PatternSyn
  (Ident2, pattern Ident2) where
import qualified Data.Text as T
import Data.Text (Text)
import Data.Function(on)

newtype Identifier = Identifier { unIdentifier :: Text }
  deriving (Show)

instance Eq Identifier where
  (==) = (==) `on` (T.toCaseFold . unIdentifier)

test = Identifier (T.pack "foo")
test0 = Identifier (T.pack "Foo")
test1 = Identifier (T.pack "Foo1")
test2 = unIdentifier test
test3 = test == test1

idLength1 :: Identifier -> Int
idLength1 = T.length . unIdentifier
idLength2 :: Identifier -> Int
idLength2 (Identifier s) = T.length s

data Ident = Ident
  { unIdent :: Text
  , folded  :: Text
  } deriving Show
makeIdent :: Text -> Ident
makeIdent s = Ident
  { unIdent = s
  , folded  = T.toCaseFold s
  }
instance Eq Ident where
 (==) = (==) `on` folded

test4 = makeIdent (T.pack "foo")
test5 = makeIdent (T.pack "Foo")
test6 = unIdent test4
test7 = test4 == test5

data Ident2 = MkIdent2
  { unIdent2 :: Text
  , folded2  :: Text
  } deriving Show
instance Eq Ident2 where
 (==) = (==) `on` folded2
pattern Ident2 :: Text -> Ident2
pattern Ident2 s <- MkIdent2 s _ where
  Ident2 s = MkIdent2 s (T.toCaseFold s)
test8 = Ident2 (T.pack "Foo")

-- https://mpickering.github.io/posts/2015-12-12-pattern-synonyms-8.html
-- https://stackoverflow.com/questions/14955627/shorthand-way-for-assigning-a-single-field-in-a-record-while-copying-the-rest-o

data Zero = Point { x :: Int, y :: Int}
  deriving Show
zero1 = Point 0 0
zero2 = Point { x = 1, y = 0}
isZero (Point 0 0) = True
isZero _ = False
zero3 = isZero zero1
zero4 = isZero zero2
getX (Point x _) = x
getX2 Point {x} = x
zero5 = getX2 zero2
updateX zero = zero {x=2}
zero6 = updateX zero2
zero7 = x zero2

pattern MyJust :: a -> Maybe a
pattern MyJust a = Just a
pattern MyPoint :: Int -> Int -> (Int, Int)
pattern MyPoint{m, n} = (m,n)
test9 = (0,0) { m = 5 }

-- https://stackoverflow.com/questions/68245095/haskell-pattern-synonyms-view-patterns-gadts#comment120625597_68247742
-- -- `data NotTrue` required because otherwise
-- -- testIsTrue cannot find data constructor
-- -- NotTrue:
data NotTrue = NotTrue | VeryTrue
  deriving (Show, Read)
-- -- no need for `Read a`, only trying to make
-- -- `isTrue` bidirectional:
pattern IsTrue :: (Read a, Show a) => a
-- pattern IsTrue <- ((== "NotTrue") . show -> False)
pattern IsTrue <- ((== "True") . show -> True)
  where IsTrue = read "True" -- no need for this line
testIsTrue IsTrue = putStrLn "it is true"
testIsTrue _ = putStrLn "not true"
main = do
  testIsTrue NotTrue
  testIsTrue True
  testIsTrue 42
  -- IsTrue True -- cannot Show, Read functions
  -- -- IsTrue is a pattern function
-- GHCI output for "NotTrue", False:
-- not true
-- it is true
-- it is true
-- GHCI output for "True", True:
-- not true
-- it is true
-- not true

data T where
  MkT :: (Show b) => b -> T
deriving instance Show T
pattern ExNumPat :: () => Show b => b -> T
pattern ExNumPat x = MkT x
test11 = ExNumPat "True"
-- output: MkT "True"
-- -- these don't seem to work:
-- testExNumPat :: (forall b. Show b => b -> T)
-- testExNumPat (ExNumPat (1::Int)) = MkT 1
-- test12 = testExNumPat (ExNumPat 1)

