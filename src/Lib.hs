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
import Search
import Data.List.Split

createEmptyDGraph :: DGraph String ()
createEmptyDGraph = insertEdgePairs [] empty

parseAndPageRank :: IO [String]
parseAndPageRank = do
  let graph = createEmptyDGraph
  let mapEmpty = Map.empty
  file <- openFile "collection.jl" ReadMode
  (graphComplete, mapWords)<-readLineByLine file graph mapEmpty
  hClose file

  --create graph from urls
  let numberOfEdges = order graphComplete

  let edges = fromStringToMap (vertices graphComplete)

  --initialize page rank
  let initValue = normalize numberOfEdges edges
  --count page rank
  let pagerankValues = handlePageRank edges initValue graphComplete 1

  let sortedPR = sortPageRank (Map.toList pagerankValues)

  let pageRank = createPageRank sortedPR
  encodeToJson pageRank

  let pageRankArr = fromTupleToString sortedPR

  writeMap (sortMap mapWords pageRankArr)

  return pageRankArr

searching:: [String] -> [String] -> IO ()
searching words pageRank = do
  putStrLn "\nType 'exit' to exit.\nType words to search.\n"
  input <- getLine
  let inputWords = splitOn " " input
  if input == "exit"
    then return ()
    else do
      result <- searchHandle inputWords words []
      let resultFinal = sortAlong pageRank result
      if null resultFinal
        then do
          putStrLn "No result"
          searching words pageRank
        else do
          putStrLn "Result:"
          --print first 10 results
          let resultList = take 10 (reverse resultFinal)
          mapM_ print resultList
          searching words pageRank


readPGfromFileOrNot :: IO [String]
readPGfromFileOrNot =  
  do
    putStrLn "\nType 'yes' if you want to read page rank from file pageRank.jsonl\nType 'no' if you want to calculate page rank\n"
    input <- getLine
    if input == "yes"      
      then do
        file <- openFile "pageRank.jsonl" ReadMode
        let pgArr=[]
        pgArrs <- readPGbyLine file pgArr
        return pgArrs
    else if input == "no" 
      then do
        pgArr <- parseAndPageRank
        return pgArr
    else do
      putStrLn "Wrong input"
      readPGfromFileOrNot

projectFunc :: IO ()
projectFunc = do
  setLocaleEncoding utf8
  pageRankArr <- readPGfromFileOrNot
  wordsInCsv <- readCsv
  searching wordsInCsv pageRankArr
  return ()