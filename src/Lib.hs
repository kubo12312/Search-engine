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

createEmptyDGraph :: DGraph String ()
createEmptyDGraph = insertEdgePairs [] empty

projectFunc :: IO ()
projectFunc = do
  let graph = createEmptyDGraph
  file <- openFile "data.jl" ReadMode
  graphComplete<-readLineByLine file graph
  hClose file

  -- -- let links = ["A" --> "B", "B" --> "C", "C" --> "C", "C" --> "B", "B" --> "A"]

  --create graph from urls
  let numberOfEdges = order graphComplete

  let edges = fromStringToMap (vertices graphComplete)

  --initialize page rank
  let initValue = normalize numberOfEdges edges
  --count page rank
  let pagerankValues = handlePageRank edges initValue graphComplete

  let sortedPR = sortPageRank (Map.toList pagerankValues)

  constructJson sortedPR

  return ()