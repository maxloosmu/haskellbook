-- https://haskell-explained.gitlab.io/blog/posts/2019/08/27/pattern-synonyms/index.html
{-# LANGUAGE PatternSynonyms #-}
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


