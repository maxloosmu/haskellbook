import Control.Monad

-- https://github.com/osfameron/words

data Cell = Cell (Integer, Integer) Char
            deriving (Eq, Ord, Show)

-- sample coordinate grid
coords = [ [(0,0),(0,1),(0,2),(0,3),(0,4),(0,5),(0,6),(0,7)]
         , [(1,0),(1,1),(1,2),(1,3),(1,4),(1,5),(1,6),(1,7)]
         , [(2,0),(2,1),(2,2),(2,3),(2,4),(2,5),(2,6),(2,7)]
         , [(3,0),(3,1),(3,2),(3,3),(3,4),(3,5),(3,6),(3,7)]
         , [(4,0),(4,1),(4,2),(4,3),(4,4),(4,5),(4,6),(4,7)]
         , [(5,0),(5,1),(5,2),(5,3),(5,4),(5,5),(5,6),(5,7)]
         , [(6,0),(6,1),(6,2),(6,3),(6,4),(6,5),(6,6),(6,7)]
         , [(7,0),(7,1),(7,2),(7,3),(7,4),(7,5),(7,6),(7,7)]
         ]

-- copy of word grid
grid = [ "__C________R___"
       , "__SI________U__"
       , "__HASKELL____B_"
       , "__A__A_____S__Y"
       , "__R___B___C____"
       , "__PHP____H_____"
       , "____S_LREP_____"
       , "____I__M_Y__L__"
       , "____L_E__T_O___"
       , "_________HB____"
       , "_________O_____"
       , "________CN_____"
       ]

-- variant of outputGrid, for arbitrary Show-able structures
og :: Show a => [a] -> IO ()
og = putStrLn . unlines . map show

-- check if divisible by 2
div2 x = x `mod` 2 == 0

-- List monad notation
mapped = do 
       i <- [0..9]
       return (i * 2)

filtered = do
       i <- [0..9]
       guard (div2 i)
       return i

mapAndFilter = do
       i <- [0..9]
       guard (div2 i)
       return (i * 5)

listcomprehension = [ i * 5 | i <- [0..9], div2 i]

coords2 = do
       row <- [0..7]
       return $ do
         col <- [0..7]
         return (row, col)

coords3 = [[(row, col) | col <- [0..7]] | row <- [0..7]]

repeat8 = take 8 . repeat
cols8 = repeat8 [0..7]
rows8 = map repeat8 [0..7]
zipOverGrid = zipWith zip
zipOverGridWith f a b = (zipWith (zipWith f)) a b
-- zipOverGridWith = zipWith . zipWith
grid8 = zipOverGrid rows8 cols8

cols = repeat [0..]
rows = map repeat [0..]
coordsInf = zipOverGrid rows cols

numberedGrid = zipOverGrid coordsInf grid
