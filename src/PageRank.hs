module PageRank where
import Data.Graph.DGraph
import Data.Graph.Types
import Data.List
import Data.Ord

{- PageRank functions -}
--change all second values for all tuples in array
initPageRank :: [(String, Float)] -> Int -> [(String, Float)]
initPageRank x order = map (\(a, b) -> (a, 1.0)) x

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
countReachable :: DGraph String () -> [(String, Float)] -> String -> Float
countReachable g edges node =
  let
    number = length (reachableAdjacentVertices g node)
    rank = findRank node edges
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

countPageRank :: Float -> [(String, Float)] -> DGraph String () -> [String] -> IO [(String, Float)]
countPageRank d edges graph nodes = do
  if null nodes
    then
      return edges
  else do
    let url = head nodes
    let allNeighbors = adjacentVertices graph url
    let reachableNeighbors = reachableAdjacentVertices graph url

    let source = allNeighbors \\ reachableNeighbors
    let source1 = source ++ isReachable graph reachableNeighbors url []
    let count = map (countReachable graph edges) source1
    let sumCount = sum count

    let newRank = (1-d) + (d * sumCount)
    let newRankValues = changeRank url newRank edges
    countPageRank d newRankValues graph (drop 1 nodes)

--check correlation of each value
isCorrelation :: [(String, Float)] -> [(String, Float)] -> Bool
isCorrelation oldPR newPR = do
  null oldPR || (do
      let oldRank = snd (head oldPR)
      let newRank = snd (head newPR)
      let diff = abs (oldRank - newRank)
      diff <= 10e-7 && isCorrelation (drop 1 oldPR) (drop 1 newPR))

handlePageRank :: [(String, Float)] -> [(String, Float)] -> DGraph String () -> IO [(String, Float)]
handlePageRank oldValuesPR newValuesPR graph = do
  let correlation = isCorrelation oldValuesPR newValuesPR
  if correlation
    then
      return newValuesPR
  else do
    newPR<-countPageRank 0.85 newValuesPR graph (vertices graph)
    handlePageRank newValuesPR newPR graph

--sort array of tuples by second element
sortPageRank :: [(String, Float)] -> [(String, Float)]
sortPageRank = sortBy (comparing snd)

--from array of string to tuple of string and float
fromStringToTuple :: [String] -> [(String, Float)]
fromStringToTuple = map (\a -> (a, 0))