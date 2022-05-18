{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# LANGUAGE DeriveAnyClass #-}

module Parser where

import Data.Aeson
import Data.ByteString as S (ByteString)
import Data.ByteString.Char8 as C8 (pack)
import Data.ByteString.Lazy.Internal as L
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
import Indexer
import qualified Data.Map as Map


data Page = Page
  { url :: String,
    html_content :: String
  }
  deriving (Generic)

instance FromJSON Page

data PageRank = PageRank
  { urlpg :: String,
    pagerank :: Double
  }
  deriving (Generic, ToJSON, FromJSON)

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

--change uppercase to lowercase for every word in [string]
lowercase :: [String] -> [String]
lowercase = map (map toLower)

--remove duplicates from list
removeDuplicates :: [String] -> [String]
removeDuplicates = nub

--sort list
sortList :: [String] -> [String]
sortList = sort

--remove empty strings from list
removeEmptyStrings :: [String] -> [String]
removeEmptyStrings = filter (not . null)

cleanUrl :: String -> [String] -> [(String, String)]
cleanUrl url x = [(url, x) | x <- x, "http" `isPrefixOf` x]

cleanBody :: [String] -> [String]
cleanBody x = [x | x <- x, not ("{{" `isPrefixOf` x), not (" {{" `isPrefixOf` x)]

strToBS :: String -> L.ByteString
strToBS = L.packChars

insertToGraph :: DGraph String () -> [(String, String)] -> DGraph String ()
insertToGraph graph x = insertEdgePairs x graph

printUrl :: Page -> IO [(String, String)]
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
    let bodyNoUppercase = lowercase bodyWords
    let bodyNoDuplicates = removeDuplicates bodyNoUppercase
    let bodySorted = sortList bodyNoDuplicates
    let bodyNoEmptyStrings = removeEmptyStrings bodySorted
    return bodyNoEmptyStrings

handleLine :: Handle -> IO ([(String, String)], [String], String)
handleLine input =
  do
    jsonLine <- hGetLine input
    let lineBS = strToBS jsonLine
    let mm = decode lineBS :: Maybe Page
    case mm of
      Nothing -> return ([], [], "")
      Just m -> do
        urls <- printUrl m
        body <- printBody m
        return (urls, body, url m)

readLineByLine :: Handle -> DGraph String () -> Map.Map String [String] -> Int -> IO (DGraph String (), Map.Map String [String])
readLineByLine input graph map i =
  do
    line <- hIsEOF input
    if line || i > 1000
      then
        return (graph, map)
      else do
        (cleanurls, body, url) <- handleLine input
        if null cleanurls || null body || url == ""
          then do
            readLineByLine input graph map (i + 1)
          else do
            let graph1 = insertToGraph graph cleanurls
            let newMap = insertMap body url map
            readLineByLine input graph1 newMap (i + 1)


handlePGLine :: Handle -> [String] -> IO [String]
handlePGLine input pgUrl=
  do
    jsonLine <- hGetLine input
    let lineBS = strToBS jsonLine
    let mm = decode lineBS :: Maybe PageRank
    case mm of
      Just m -> do
        let urlx=[urlpg m]
        let pgUrls = pgUrl ++ urlx
        return pgUrls
      Nothing -> do
        return pgUrl

readPGbyLine :: Handle -> [String] -> IO [String]
readPGbyLine input pgUrl =
  do
    line <- hIsEOF input
    if line
      then
        return pgUrl
      else do
        pgUrls <- handlePGLine input pgUrl
        readPGbyLine input pgUrls

--from [L.ByteString] to String with new line
bsToStr :: [L.ByteString] -> String
bsToStr = unlines . map L.unpackChars

constructJson :: [(String, Double)] -> IO ()
constructJson [] = return ()
constructJson (x:xs) = do
  let json = "{\"urlpg\": \"" ++ fst x ++ "\",\"pagerank\": " ++ show (snd x) ++ "}\n"
  appendFile "pageRank.jsonl" json
  constructJson xs