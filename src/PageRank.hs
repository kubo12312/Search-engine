{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use tuple-section" #-}
module PageRank where
import Data.Graph.DGraph
import Data.Graph.Types
import Data.List
import Data.Ord
import Data.Map (Map, (!))
import qualified Data.Map as Map

{- PageRank functions -}
--change all second values in map (string double) with 1/n where n is parameter
normalize :: Int -> Map String Double -> Map String Double
normalize n = Map.map (\x -> 1 / fromIntegral n)

--find in array of tuples the first element and return the second element
findRank :: Eq a => a -> [(a, b)] -> b
findRank x = snd . head . filter (\(a, b) -> a == x)

--append to array of Int
append :: [Int] -> Int -> [Int]
append x y = x ++ [y]

--find in array of tuples by first element and change the second element, copy all other values
changeRank :: Eq a => a -> b -> [(a, b)] -> [(a, b)]
changeRank x y = map (\(a, b) -> if a == x then (a, y) else (a, b))

--count all reachable nodes from a node
countReachable :: DGraph String () -> Map String Double -> String -> Double
countReachable g edges node =
  let
    number = length (reachableAdjacentVertices g node)
    rank = edges ! node
    total = rank / fromIntegral number
  in total

--check if node is reachable from node
isReachable :: DGraph String () -> [String] -> String -> [String] -> [String]
isReachable graph nodes node nodesReach = do
  let reachable = reachableAdjacentVertices graph (head nodes)
  if null nodes
    then nodesReach
    else do
      if node `elem` reachable
        then isReachable graph (drop 1 nodes) node (nodesReach ++ [head nodes])
        else isReachable graph (drop 1 nodes) node nodesReach

countPageRank :: Double -> Map String Double -> DGraph String () -> [String] -> Map String Double
countPageRank d edges graph nodes = do
  if null nodes
    then
      edges
  else do
    let url = head nodes
    let allNeighbors = adjacentVertices graph url
    let reachableNeighbors = reachableAdjacentVertices graph url

    let source = allNeighbors \\ reachableNeighbors
    let source1 = source ++ isReachable graph reachableNeighbors url []
    let count = map (countReachable graph edges) source1
    let sumCount = sum count

    let newRank = (1-d) + (d * sumCount)
    --update value in map with key
    let newRankValues = Map.insert url newRank edges

    countPageRank d newRankValues graph (drop 1 nodes)

--check difference between every second value in map with second value in other map, if difference is less than 10e-7, return true
isEqual :: Map String Double -> Map String Double -> Bool
isEqual a b =
  let
    aList = Map.toList a
    bList = Map.toList b
  in
    all (\(x, y) -> abs (y - (b ! x)) < 10e-7) aList

handlePageRank :: Map String Double -> Map String Double -> DGraph String () -> Map String Double
handlePageRank oldValuesPR newValuesPR graph = do
  let correlation = isEqual oldValuesPR newValuesPR
  if correlation
    then
      newValuesPR
  else do
    let newPR = countPageRank 0.85 newValuesPR graph (vertices graph)
    handlePageRank newValuesPR newPR graph

--sort array of tuples by second element
sortPageRank :: [(String, Double)] -> [(String, Double)]
sortPageRank = sortBy (comparing snd)

--from array of string to tuple of string and float
fromStringToTuple :: [String] -> [(String, Double)]
fromStringToTuple = map (\a -> (a, 0))

--from array of string to map of string and float
fromStringToMap :: [String] -> Map String Double
fromStringToMap = Map.fromList . fromStringToTuple