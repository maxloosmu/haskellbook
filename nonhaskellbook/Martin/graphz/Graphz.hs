{-# OPTIONS_GHC -XFlexibleInstances #-}

module Graphz where

-- import Data.Maybe
import Data.Text.Lazy as T (pack)
-- import Data.List

import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.PatriciaTree

import Data.GraphViz
-- import Data.GraphViz.Printing
-- import Data.GraphViz.Commands
import Data.GraphViz.Attributes.Complete

smallGrNodes :: [LNode String]
smallGrNodes = [(0, "A"), (1, "B"), 
  (2, "C"), (3, "G")]

smallGrEdges :: [LEdge Int]
smallGrEdges = [(0, 1, 3), (1, 2, 4), 
  (1, 3, 2), (2, 3, 5), (3, 2, 5)]

smallGraph :: Gr String Int
smallGraph = mkGraph smallGrNodes 
  smallGrEdges

smallGrDot :: IO FilePath
smallGrDot = runGraphviz
  (graphToDot quickParams smallGraph) 
  Pdf "graph.pdf"

largeGrNodes :: [LNode String]
largeGrNodes = [(1, "A"), (2, "B"), 
  (3, "C"), (4, "D"), (5, "E"), 
  (6, "F"), (7, "G"), (8, "H"), 
  (9, "I"), (10, "J")]

largeGrEdges :: [LEdge String]
largeGrEdges = [(1, 2, ""), 
  (1, 5, ""), (1, 4, ""),
  (4, 8, ""), (5, 8, ""),
  (5, 9, ""), (2, 7, ""),
  (2, 3, ""), (3, 7, ""),
  (3, 6, ""), (3, 9, ""),
  (9, 10, ""), (6, 10, "")]

largeGraph :: Gr String String
largeGraph = mkGraph largeGrNodes 
  largeGrEdges

largeGrDot :: IO FilePath
largeGrDot = runGraphviz
  (graphToDot quickParams largeGraph) 
  Pdf "graph2.pdf"

data EdgeListGraph a = ELG [a] [(a, a)]
  deriving (Eq, Ord, Show , Read)


smallGrELG :: EdgeListGraph String
smallGrELG = ELG 
  ["A", "B", "C", "G"] 
  [("A", "B"), ("B", "C"), 
  ("B", "G"), ("C", "G"), 
  ("G", "C")]
edgeListGraphToGrNodes :: 
  [a] -> [LNode a]
edgeListGraphToGrNodes xs = 
  zip [1..] xs
swapped :: [LNode String]
swapped = edgeListGraphToGrNodes 
  ["A", "B", "C", "G"]
edgeListGraphToGrEdges ::
  [(String, String)] -> [LEdge String]
edgeListGraphToGrEdges [] = []
edgeListGraphToGrEdges (y:ys) = 
  [(fst n1, fst n2,"") | n1 <- swapped,
  snd n1 == fst y, n2 <- swapped,
  snd n2 == snd y] ++ 
  edgeListGraphToGrEdges ys
test :: [LEdge String]
test = edgeListGraphToGrEdges 
  [("B", "C")]
edgeListGraphToGr :: (Eq a) =>
  EdgeListGraph a -> Gr a String
edgeListGraphToGr (ELG xs zs) = 
  let swap = edgeListGraphToGrNodes xs
      eLGTGEdges [] = []
      eLGTGEdges (y:ys) = 
        [(fst n1, fst n2,"") | n1 <- swap,
        snd n1 == fst y, n2 <- swap,
        snd n2 == snd y] ++ 
        eLGTGEdges ys
  in mkGraph swap (eLGTGEdges zs)
sGNodes :: [String]
sGNodes = ["A", "B", "C", "G"]
sGEdges :: [(String, String)]
sGEdges = [("A","B"), ("B","C"), ("B","G"), 
  ("C","G"), ("G","C")]
test2 :: Gr String String
test2 = edgeListGraphToGr 
  (ELG sGNodes sGEdges)
test3 :: IO FilePath
test3 = runGraphviz
  (graphToDot quickParams test2) 
  Pdf "graph3.pdf"


data NextFunGraph a = 
  NFG [a] (a -> [a])
smallGrNexts :: String -> [String]
smallGrNexts x = case x of
  "A" -> ["B"]
  "B" -> ["C", "G"]
  "C" -> ["G"]
  "G" -> ["C"]
  _ -> []
smallGrNFG :: NextFunGraph String
smallGrNFG = NFG ["A", "B", "C", "G"] 
  smallGrNexts
edgeNextsToEdgeList :: 
  (a -> [a]) -> [a] -> [(a, a)]
edgeNextsToEdgeList _ [] = []
edgeNextsToEdgeList f (x:xs) = 
  [(x, y) | y <- f x] ++
  edgeNextsToEdgeList f xs
test4 :: [(String, String)]
test4 = edgeNextsToEdgeList
  smallGrNexts sGNodes
nextFunGraphToEdgeListGraph :: 
  NextFunGraph a -> EdgeListGraph a
nextFunGraphToEdgeListGraph (NFG xs f) = 
  ELG xs (edgeNextsToEdgeList f xs)
test5 :: EdgeListGraph String
test5 = nextFunGraphToEdgeListGraph
  smallGrNFG
smallGraph2 :: Gr String String
smallGraph2 =
  edgeListGraphToGr 
  (nextFunGraphToEdgeListGraph
  (NFG ["A", "B", "C", "G"] 
  smallGrNexts))
smallGrDot2 :: IO FilePath
smallGrDot2 = runGraphviz
  (graphToDot quickParams smallGraph2) 
  Pdf "graph4.pdf"


breadthfirst :: (t -> [t]) -> 
  (t -> Bool) -> t -> [t]
breadthfirst nexts sol x = bfs [x]
  where
    bfs [] = []
    bfs (nd:nds) =
      if sol nd
      then nd : bfs (nds ++ nexts nd)
      else bfs (nds ++ nexts nd)
depthfirst :: (t -> [t]) -> 
  (t -> Bool) -> t -> [t]
depthfirst nexts sol x = dfs [x]
  where
    dfs [] = []
    dfs (nd:nds) =
      if sol nd
      then nd : dfs (nexts nd ++ nds)
      else dfs (nexts nd ++ nds)

test6 :: [String]
test6 = take 5 (depthfirst 
  smallGrNexts (== "G") "A")
type SearchPath a = [a]
liftNextsToPath :: (a -> [a]) -> 
  SearchPath a -> [SearchPath a]
liftNextsToPath f xs = [xs ++ [x] | 
  x <- f (last xs)]
-- -- this works by getting vs code
-- -- to suggest solutions for _, but
-- -- it is not a solution:
-- liftNextsToPath2 :: [[String]]
-- liftNextsToPath2 = map ((["A", "B"] ++) .
--   _) ["C", "G"]
-- -- map words ["C D", "G"] will 
-- -- split C and D, cannot use words,
-- -- this works and can be a solution:
-- liftNextsToPath3 :: [[String]]
-- liftNextsToPath3 = map (["A", "B"] ++) 
--   (map (:[]) ["C", "G"])
-- -- this works and outputs: ["C"]
-- test7 :: [String]
-- test7 =  "C" :[]
-- -- this works, but adding no 
-- -- duplication may be very difficult:
liftNextsToPath4 :: (a -> [a]) -> 
  [a] -> [[a]]
liftNextsToPath4 f xs = 
  map ((xs ++) . (:[])) (f (last xs))
test8 :: [SearchPath String]
test8 = liftNextsToPath 
  smallGrNexts ["A", "B"]
-- changed liftNextsToPath to
-- liftNextsToPathNoDup
pathsFromToBFS :: Eq a => 
  (a -> [a]) -> a -> a -> 
  [SearchPath a]
pathsFromToBFS nexts start goal =
  breadthfirst
  (liftNextsToPathNoDup2 nexts)
  (\pth -> last pth == goal)
  [start]
pathsFromToDFS :: Eq a => 
  (a -> [a]) -> a -> a -> 
  [SearchPath a]
pathsFromToDFS nexts start goal =
  depthfirst
  (liftNextsToPathNoDup2 nexts)
  (\pth -> last pth == goal)
  [start]
test9 :: [SearchPath String]
test9 = take 5 (pathsFromToDFS 
  smallGrNexts "A" "G") 
liftNextsToPathNoDup :: (Eq a) =>
  (a -> [a]) -> SearchPath a ->
  [SearchPath a]
liftNextsToPathNoDup f xs = [xs ++ [x] | 
  x <- f (last xs), x `notElem` xs]

-- Martin's Notes:
-- concatMap :: Foldable t => 
--   (a -> [b]) -> t a -> [b]
-- When instantiating t with lists, 
-- we get: concatMap ::  (a -> [b]) ->  
-- [a] -> [b] . The argument of concatMap 
-- is of type String -> [[String]] in the
-- examples, so the type parameter a 
-- becomes instantiated with String and 
-- type parameter b with [String], so for
-- an argument [String] (= SearchPath 
-- String), the result has type [[String]]
-- ( = [SearchPath String] ).
-- My Notes:
-- n is String, [pth ++ [n]] is [[String]]
-- so (\n -> if n `elem` pth then [] 
--   else [pth ++ [n]]) 
-- will be String -> [[String]]
liftNextsToPathNoDup2 :: (Eq a) =>
  (a -> [a]) -> SearchPath a ->
  [SearchPath a]
liftNextsToPathNoDup2 nxts pth = 
  concatMap
  (\n -> if n `elem` pth then [] 
    else [pth ++ [n]])
  (nxts (last pth))



data Side = Ls | Rs
  deriving (Eq, Ord, Show , Read)
type CfgwState = (Side , Side , Side , Side)
crossCabbage :: (Side, b, c, d) -> 
  (Side, b, c, d)
crossCabbage (c, f, g, w) = 
  if c == Ls then (Rs, f, g, w)
  else (Ls, f, g, w)
crossFerryman :: (a, Side, c, d) -> 
  (a, Side, c, d)
crossFerryman (c, f, g, w) = 
  if f == Ls then (c, Rs, g, w)
  else (c, Ls, g, w)
crossGoat :: (a, b, Side, d) -> 
  (a, b, Side, d)
crossGoat (c, f, g, w) = 
  if g == Ls then (c, f, Rs, w)
  else (c, f, Ls, w)
crossWolf :: (a, b, c, Side) -> 
  (a, b, c, Side)
crossWolf (c, f, g, w) = 
  if w == Ls then (c, f, g, Rs)
  else (c, f, g, Ls)

validState :: CfgwState -> Bool
validState (Ls, Rs, Ls, _) = False
validState (Rs, Ls, Rs, _) = False
validState (_, Rs, Ls, Ls) = False
validState (_, Ls, Rs, Rs) = False
validState (_, _, _, _) = True
cross :: [(Side, Side, Side, Side) -> 
  (Side, Side, Side, Side)]
cross = [crossCabbage, crossGoat, 
  crossWolf]
max2cross :: [(Side, Side, Side, Side) 
  -> (Side, Side, Side, Side)]
max2cross = [x . crossFerryman | 
  x <- cross] ++ [crossFerryman]
crossBoat :: CfgwState -> [CfgwState]
crossBoat s = filter validState $ 
  max2cross <*> [s]
test10 :: [CfgwState]
test10 = crossBoat (Ls, Ls, Ls, Ls)
test11 :: [SearchPath CfgwState]
test11 = take 3 (pathsFromToBFS 
  crossBoat (Ls, Ls, Ls, Ls) 
  (Rs, Rs, Rs, Rs))
test12 :: [SearchPath CfgwState]
test12 = pathsFromToDFS 
  crossBoat (Ls, Ls, Ls, Ls) 
  (Rs, Rs, Rs, Rs)

-- graph5.pdf, graph7.pdf previously 
-- require MAC computer, now Windows 
-- is ok, not sure why..
rGNodes :: [String]
rGNodes = ["(Ls,Ls,Ls,Ls)", 
  "(Rs,Ls,Ls,Ls)", "(Ls,Rs,Ls,Ls)", 
  "(Ls,Ls,Rs,Ls)", "(Ls,Ls,Ls,Rs)",
  "(Rs,Rs,Ls,Ls)", "(Ls,Rs,Rs,Ls)",
  "(Ls,Ls,Rs,Rs)", "(Rs,Ls,Ls,Rs)",
  "(Rs,Ls,Rs,Ls)", "(Ls,Rs,Ls,Rs)",
  "(Ls,Rs,Rs,Rs)", "(Rs,Ls,Rs,Rs)",
  "(Rs,Rs,Ls,Rs)", "(Rs,Rs,Rs,Ls)",
  "(Rs,Rs,Rs,Rs)"]
rGrNexts :: String -> [String]
rGrNexts x = case x of
  "(Ls,Ls,Ls,Ls)" -> ["(Ls,Rs,Rs,Ls)"]
  "(Ls,Rs,Rs,Ls)" -> ["(Ls,Ls,Rs,Ls)"]
  "(Ls,Ls,Rs,Ls)" -> ["(Rs,Rs,Rs,Ls)", 
    "(Ls,Rs,Rs,Rs)"]
  "(Rs,Rs,Rs,Ls)" -> ["(Rs,Ls,Ls,Ls)"]
  "(Rs,Ls,Ls,Ls)" -> ["(Rs,Rs,Ls,Rs)"]
  "(Rs,Rs,Ls,Rs)" -> ["(Rs,Ls,Ls,Rs)"]
  "(Rs,Ls,Ls,Rs)" -> ["(Rs,Rs,Rs,Rs)"]
  "(Ls,Rs,Rs,Rs)" -> ["(Ls,Ls,Ls,Rs)"]
  "(Ls,Ls,Ls,Rs)" -> ["(Rs,Rs,Ls,Rs)"]
  _ -> []
riverGraph :: Gr String String
riverGraph =
  edgeListGraphToGr 
  (nextFunGraphToEdgeListGraph
  (NFG rGNodes 
  rGrNexts))
riverGrDot :: IO FilePath
riverGrDot = runGraphviz
  (graphToDot quickParams riverGraph) 
  Pdf "graph5.pdf"

-- rGNodes2 :: [CfgwState]
-- rGNodes2 = [(Ls,Ls,Ls,Ls), 
--   (Rs,Ls,Ls,Ls), (Ls,Rs,Ls,Ls), 
--   (Ls,Ls,Rs,Ls), (Ls,Ls,Ls,Rs),
--   (Rs,Rs,Ls,Ls), (Ls,Rs,Rs,Ls),
--   (Ls,Ls,Rs,Rs), (Rs,Ls,Ls,Rs),
--   (Rs,Ls,Rs,Ls), (Ls,Rs,Ls,Rs),
--   (Ls,Rs,Rs,Rs), (Rs,Ls,Rs,Rs),
--   (Rs,Rs,Ls,Rs), (Rs,Rs,Rs,Ls),
--   (Rs,Rs,Rs,Rs)]
-- -- not possible to use rGrNexts2 or 
-- -- pathsFromToBFS to replace crossBoat
-- rGrNexts2 :: [SearchPath CfgwState]
-- rGrNexts2 = pathsFromToBFS 
--   crossBoat (Ls, Ls, Ls, Ls) 
--   (Rs, Rs, Rs, Rs)
-- riverGraph2 :: Gr CfgwState String
-- riverGraph2 =
--   edgeListGraphToGr 
--   (nextFunGraphToEdgeListGraph
--   (NFG rGNodes2 
--   crossBoat))
-- -- crossBoat in riverGraph2 doesn't
-- -- work, leads to quickParams error in 
-- -- riverGrDot2, graph6.pdf cannot be 
-- -- created:
-- riverGrDot2 :: IO FilePath
-- riverGrDot2 = runGraphviz
--   (graphToDot quickParams riverGraph2) 
--   Pdf "graph6.pdf"

cfgwNodes :: [CfgwState]
cfgwNodes = [(Ls,Ls,Ls,Ls), 
  (Rs,Ls,Ls,Ls), (Ls,Rs,Ls,Ls), 
  (Ls,Ls,Rs,Ls), (Ls,Ls,Ls,Rs),
  (Rs,Rs,Ls,Ls), (Ls,Rs,Rs,Ls),
  (Ls,Ls,Rs,Rs), (Rs,Ls,Ls,Rs),
  (Rs,Ls,Rs,Ls), (Ls,Rs,Ls,Rs),
  (Ls,Rs,Rs,Rs), (Rs,Ls,Rs,Rs),
  (Rs,Rs,Ls,Rs), (Rs,Rs,Rs,Ls),
  (Rs,Rs,Rs,Rs)]
cfgwGraph :: Gr CfgwState String
cfgwGraph = edgeListGraphToGr 
  (nextFunGraphToEdgeListGraph 
  (NFG cfgwNodes crossBoat))
cfgwDot :: IO FilePath
cfgwDot = runGraphviz 
  (graphToDot quickParams cfgwGraph) 
  Pdf "graph7.pdf"
instance Labellable CfgwState where
  toLabelValue s = StrLabel (T.pack (show s))


-- -- these don't seem to have a future:
-- chessGrNodes :: [LNode Int]
-- chessGrNodes = [(0, 1), (1, 2), 
--   (2, 3), (3, 4)
--   ]
-- type BoardSize = Int
-- type BoardState = [(BoardSize, BoardSize)]
-- type ChessState = [Int]
-- boardSize :: BoardSize
-- boardSize = 4
-- xAxis :: [BoardSize]
-- xAxis = [1..boardSize]
-- yAxis :: [BoardSize]
-- yAxis = [1..boardSize]
-- boardState :: BoardState
-- boardState = zip xAxis yAxis
-- validY :: Int -> Int -> Bool
-- validY y1 y2
--   | y1 == y2 = False
--   | otherwise = True
-- reduceY :: Int -> [Int] -> [Int]
-- reduceY y = filter (/= y)

-- -- this doesn't work very well:
-- reduceY :: Eq a => a -> [a] -> [a]
-- reduceY y = filter (/= y)
-- computeY :: [[Int]] -> [[Int]]
-- computeY [] = []
-- computeY [y1] = [y1]
-- computeY (y1:y2:ys) = reduceY y1 
--   (filter validY2 (y1:y2:ys)) where
--     validY2 [] = False
--     validY2 [_] = True
--     validY2 (y3:y4:_)
--       | y3 == y4 = False
--       | otherwise = True
-- test13 :: [[BoardSize]]
-- test13 = computeY [yAxis]


-- wiki.haskell.org/99_questions/Solutions/90
queen :: Int -> [[Int]]
queen n = generate n where 
  generate 0 = [[]]
  generate k = [q : qs | q <- [1..n], 
    qs <- generate (k-1)]
queen2 :: Int -> [[Int]]
queen2 n = filter check (queen n) where
  check [] = True
  check (q:qs) = isSafe q qs && check qs
  isSafe try qs = try `notElem` qs &&
    not (sameDiag try qs)
  sameDiag try qs = any (\(colDist, q2)
    -> abs (try - q2) == colDist) 
    $ zip [1..] qs









