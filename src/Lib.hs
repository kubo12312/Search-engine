{-# LANGUAGE DeriveGeneric #-}
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
import Data.Map (Map, (!))
import qualified Data.Map as Map


projectFunc :: IO ()
projectFunc = do
  file <- openFile "data.jl" ReadMode
  let link = ["" --> ""]
  links<-readLineByLine file (drop 1 link)
  --print (links)
  hClose file

  -- let links = ["A" --> "B", "B" --> "C", "C" --> "C", "C" --> "B", "B" --> "A"]

  --create graph from urls
  let graph = someDirectedGraph links
  let numberOfEdges = order graph

  let edges = fromStringToMap (vertices graph)

  --initialize page rank
  let initValue = normalize numberOfEdges edges
  --count page rank
  let pagerankValues = handlePageRank edges initValue graph

  let sortedPR = sortPageRank (Map.toList pagerankValues)

  print sortedPR

  return ()
