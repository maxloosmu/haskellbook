{-# OPTIONS_GHC -XFlexibleInstances #-}

module Graph where

import Data.Maybe
import Data.Text.Lazy as T (pack)

import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.PatriciaTree

import Data.GraphViz
import Data.GraphViz.Printing
import Data.GraphViz.Commands
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

