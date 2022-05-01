{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use tuple-section" #-}

module Lib
    ( projectFunc
    ) where

import PageRank
import Parser
import System.IO
import Data.Graph.Types
import Data.Graph.DGraph
import Data.Map (Map, (!))
import qualified Data.Map as Map
import Indexer
import GHC.IO.Encoding

createEmptyDGraph :: DGraph String ()
createEmptyDGraph = insertEdgePairs [] empty

projectFunc :: IO ()
projectFunc = do
  setLocaleEncoding utf8
  let graph = createEmptyDGraph
  let mapEmpty = Map.empty
  file <- openFile "collection.jl" ReadMode
  (graphComplete, mapWords)<-readLineByLine file graph mapEmpty
  hClose file

  -- let links = [("A", "B"), ("B", "C"), ("C", "C"), ("C", "B"), ("B", "A")]
  -- let graphComplete = insertEdgePairs links graph

  --create graph from urls
  let numberOfEdges = order graphComplete

  let edges = fromStringToMap (vertices graphComplete)

  --initialize page rank
  let initValue = normalize numberOfEdges edges
  --count page rank
  let pagerankValues = handlePageRank edges initValue graphComplete 1

  let sortedPR = sortPageRank (Map.toList pagerankValues)

  constructJson sortedPR

  let pageRankArr = fromTupleToString sortedPR
  
  writeMap mapWords

  print (sortMap mapWords pageRankArr)

  return ()