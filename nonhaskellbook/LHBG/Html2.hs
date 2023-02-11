-- Html2.hs

module Html2
  ( Html
  , Title
  , Structure
  , html_
  , p_
  , h1_
  , append2_
  , render
  )
  where

newtype Html
  = Html String

type Structure
  = Structure3 String

-- https://stackoverflow.com/questions/50491498/haskell-fmap-over-custom-data-type

data Structure3 a = Structure3 a deriving Show
type Structure2 = Structure3 String

instance Functor Structure3 where
  fmap f (Structure3 a) = Structure3 (f a)

instance Applicative Structure3 where
  pure a =  Structure3 a
  (Structure3 f) <*> Structure3 a = Structure3 (f a)
  -- (<*>) = liftA2 id
  -- liftA2 Structure3 a b = Structure3 <$> a <*> b

append2_ :: Structure2 -> Structure2 -> Structure2
append2_ c1 c2 = (<>) <$> c1 <*> c2

-- append3_ :: Structure2
-- append3_ = (<>) <$> (Structure3 "try") <*> (Structure3 " test")

type Title
  = String

html_ :: Title -> Structure -> Html
html_ title content =
  Html
    ( el "html"
      ( el "head" (el "title" title)
        <> el "body" (getStructureString content)
      )
    )

p_ :: String -> Structure
p_ = Structure3 . el "p"

h1_ :: String -> Structure
h1_ = Structure3 . el "h1"

el :: String -> String -> String
el tag content =
  "<" <> tag <> ">" <> content <> "</" <> tag <> ">"

-- append_ :: Structure -> Structure -> Structure
-- append_ c1 c2 =
--   Structure (getStructureString c1 <> getStructureString c2)

getStructureString :: Structure -> String
getStructureString content =
  case content of
    Structure3 str -> str

render :: Html -> String
render html =
  case html of
    Html str -> str
