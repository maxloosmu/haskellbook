-- hello.hs

import Html2

main :: IO ()
main = putStrLn (render myhtml)

myhtml :: Html
myhtml =
  html_
    "My title"
    ( append2_
      (h1_ "Heading")
      ( append2_
        (p_ "Paragraph #1")
        (p_ "Paragraph #2")
      )
    )
