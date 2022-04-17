{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use tuple-section" #-}

module Lib
    ( projectFunc
    ) where

import Data.Aeson
import Data.ByteString as S (ByteString)
import Data.ByteString.Char8 as C8 (pack)
import Data.ByteString.Lazy.Internal as L
import Data.List
import qualified Data.Text as T
import Data.Text.ICU.Char
import Data.Text.ICU.Normalize
import GHC.Generics
import System.IO
import Text.HandsomeSoup
import Text.Regex.TDFA
import Text.XML.HXT.Core
import Zenacy.HTML
import Data.Char
import Data.Graph.DGraph
import Data.Graph.Types
import Data.Graph.Visualize
import Data.Maybe

import Data.List
import Data.Graph.Connectivity
import Data.Ord


data Page = Page
  { url :: String,
    html_content :: String
  }
  deriving (Generic)

instance FromJSON Page

canonicalForm :: String -> String
canonicalForm s = T.unpack noAccents
  where
    noAccents = T.filter (not . property Diacritic) normalizedText
    normalizedText = normalize NFD (T.pack s)

-- split string into words
split :: String -> [String]
split = words . map (\c -> if isAlphaNum c then c else ' ')

--remove newlines from string
removeNewlines :: String -> String
removeNewlines = filter (/= '\n')


--remove special characters from string
removeSpecialChars :: String -> String
removeSpecialChars = filter (\x -> x `elem` ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ [' '])

--remove uppercase from element in list
removeUppercase :: [String] -> [String]
removeUppercase = filter (not . any isUpper)

--remove duplicates from list
removeDuplicates :: [String] -> [String]
removeDuplicates = nub

--sort list
sortList :: [String] -> [String]
sortList = sort

--remove empty strings from list
removeEmptyStrings :: [String] -> [String]
removeEmptyStrings = filter (not . null)

cleanUrl :: String -> [String] -> [Arc String ()]
cleanUrl url x = [url --> x | x <- x, "http" `isPrefixOf` x]

cleanBody :: [String] -> [String]
cleanBody x = [x | x <- x, not ("{{" `isPrefixOf` x), not (" {{" `isPrefixOf` x)]

strToBS :: String -> L.ByteString
strToBS = L.packChars

someDirectedGraph :: [Arc String ()] -> DGraph String ()
someDirectedGraph edges = fromArcsList edges

printUrl :: Page -> IO [Arc String ()]
printUrl m =
  do
    let doc = readString [withParseHTML yes, withWarnings no] (html_content m)
    links <- runX $ doc >>> css "a" ! "href"

    let linksNoDuplicates = removeDuplicates links

    let linksClean = cleanUrl (url m) linksNoDuplicates
    return linksClean

printBody :: Page -> IO [String]
printBody m = do
    let doc = readString [withParseHTML yes, withWarnings no, withInputEncoding  isoLatin1] (html_content m)
    body <- runX $ doc >>> css "body" //> neg (css "script") >>> removeAllWhiteSpace //> getText
    let bodyText = unlines (map canonicalForm (cleanBody body))
    let bodyNoNewlines = removeNewlines bodyText
    let bodyNoSpecialChars = removeSpecialChars bodyNoNewlines
    let bodyWords = split bodyNoSpecialChars
    let bodyNoUppercase = removeUppercase bodyWords
    let bodyNoDuplicates = removeDuplicates bodyNoUppercase
    let bodySorted = sortList bodyNoDuplicates
    let bodyNoEmptyStrings = removeEmptyStrings bodySorted
    return bodyNoEmptyStrings

handleLine :: Handle -> [Arc String ()] -> IO [Arc String ()]
handleLine input links=
  do
    jsonLine <- hGetLine input
    let lineBS = strToBS jsonLine
    let mm = decode lineBS :: Maybe Page
    case mm of
      Nothing -> return ["error" --> "error"]
      Just m -> do
        cleanurl <- printUrl m
        let cleanurls = links ++ cleanurl
        return cleanurls

readLineByLine :: Handle -> [Arc String ()] -> IO [Arc String ()]
readLineByLine input links=
  do
    line <- hIsEOF input
    let cleanurls = links
    if line
      then
        return links
      else do
        cleanurls <- handleLine input links
        readLineByLine input cleanurls

{- PageRank functions -}
--change all second values for all tuples in array
initPageRank :: [(String, Float)] -> Int -> [(String, Float)]
initPageRank x order = map (\(a, b) -> (a, 1 / fromIntegral order)) x

--find in array of tuples the first element and return the second element
findRank :: Eq a => a -> [(a, b)] -> b
findRank x = snd . head . filter (\(a, b) -> a == x)

--append to array of Int
append :: [Int] -> Int -> [Int]
append x y = x ++ [y]

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

countPageRank :: [(String, Float)] -> DGraph String () -> (String, Float) -> IO (String,Float)
countPageRank edges graph (url, rank) = do
  let allNeighbors = adjacentVertices graph url
  let reachableNeighbors = reachableAdjacentVertices graph url

  let source = allNeighbors \\ reachableNeighbors
  let source1 = source ++ isReachable graph reachableNeighbors url []
  let count = map (countReachable graph edges) source1
  let sumCount = sum count


  let newRank = 0.15 + (0.85 * sumCount)
  return (url, newRank)

--check correlation of each value
checkDifference :: [(String, Float)] -> [(String, Float)] -> [(String, Float)]
checkDifference oldPR newPR = [(a, b) | (x, y) <- oldPR, (a, b) <- newPR, y-b>=10e-6 && a==x || b-y>=10e-6 && x==a]

--compare length of list of correlated values and list of pageranks
compareDifference :: [(String, Float)] -> IO Bool
compareDifference differencePG = do
  if null differencePG
    then
      return False
  else do
    return True

handlePageRank :: [(String, Float)] -> [(String, Float)] -> DGraph String () -> IO [(String, Float)]
handlePageRank oldValuesPR newValuesPR graph = do
  let comparedValue = checkDifference oldValuesPR newValuesPR
  compareBoolValue<-compareDifference comparedValue
  if not compareBoolValue
    then
      return newValuesPR
  else do
    --for every item in array, calculate page rank
    newPR<-mapM (countPageRank newValuesPR graph) newValuesPR
    handlePageRank newValuesPR newPR graph

--divide all values in list of tuples
divide :: [(String, Float)] -> Int -> [(String, Float)]
divide x y = map (\(a, b) -> (a, b / fromIntegral (y))) x

--sor array of tuples by second element
sortPageRank :: [(String, Float)] -> [(String, Float)]
sortPageRank = sortBy (comparing snd)

--from array of string to tuple of string and float
fromStringToTuple :: [String] -> [(String, Float)]
fromStringToTuple = map (\a -> (a, 0))

projectFunc :: IO ()
projectFunc = do
  file <- openFile "data.jl" ReadMode
  let link = ["" --> ""]
  links<-readLineByLine file (drop 1 link)
  --print (links)
  hClose file

  --create graph from urls
  let graph = someDirectedGraph links
  let numberOfEdges = order graph

  let edges = fromStringToTuple (vertices graph)

  --initialize page rank
  let initValue = initPageRank edges numberOfEdges
  --count page rank
  pagerankValues<-handlePageRank edges initValue graph
  --divide pagerank and then sort
  let sortedPR = sortPageRank (divide pagerankValues numberOfEdges)

  print sortedPR

  return ()
