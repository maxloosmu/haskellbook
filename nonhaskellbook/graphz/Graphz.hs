{-# OPTIONS_GHC -XFlexibleInstances #-}

module Graphz where

-- import Data.Maybe
-- import Data.Text.Lazy as T (pack)

import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.PatriciaTree

import Data.GraphViz
-- import Data.GraphViz.Printing
-- import Data.GraphViz.Commands
-- import Data.GraphViz.Attributes.Complete

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
test6 :: [String]
test6 = take 5 (breadthfirst 
  smallGrNexts (== "G") "A")
type SearchPath a = [a]
liftNextsToPath :: (a -> [a]) -> 
  SearchPath a -> [SearchPath a]
liftNextsToPath f xs = [xs ++ [x] | 
  x <- f (last xs)]
-- -- this works by getting vs code
-- -- to suggest solutions for _, but
-- -- it is not a solution:
liftNextsToPath2 :: [[String]]
liftNextsToPath2 = map ((["A", "B"] ++) .
  _) ["C", "G"]
-- -- map words ["C D", "G"] will 
-- -- split C and D, cannot use words,
-- -- this works and can be a solution:
-- liftNextsToPath3 :: [[String]]
-- liftNextsToPath3 = map (["A", "B"] ++) 
--   (map (:[]) ["C", "G"])
-- -- this works and outputs: ["C"]
-- test7 :: [String]
-- test7 =  "C" :[]
test8 :: [SearchPath String]
test8 = liftNextsToPath 
  smallGrNexts ["A", "B"]







