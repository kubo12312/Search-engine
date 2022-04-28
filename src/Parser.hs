{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

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