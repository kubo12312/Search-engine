{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

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

import Data.List


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

cleanUrl :: String -> [String] -> [[String]]
cleanUrl url x = [[url, x] | x <- x, "http" `isPrefixOf` x]

cleanBody :: [String] -> [String]
cleanBody x = [x | x <- x, not ("{{" `isPrefixOf` x), not (" {{" `isPrefixOf` x)]

strToBS :: String -> L.ByteString
strToBS = L.packChars



printUrl :: Page -> IO [[String]]
printUrl m =
  do
    --putStrLn $ url m
    let doc = readString [withParseHTML yes, withWarnings no] (html_content m)
    links <- runX $ doc >>> css "a" ! "href"

    let linksNoDuplicates = removeDuplicates links

    let linksClean = cleanUrl (url m) linksNoDuplicates
    return linksClean
    --print linksClean

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

handleLine :: Handle -> [[String]] -> IO [[String]]
handleLine input links=
  do
    jsonLine <- hGetLine input
    let lineBS = strToBS jsonLine
    let mm = decode lineBS :: Maybe Page
    case mm of
      Nothing -> return [["error parsing JSON"]]
      Just m -> do
        cleanurl <- printUrl m
        let cleanurls = links ++ cleanurl 
        return cleanurls             

readLineByLine :: Handle -> [[String]] -> IO [[String]]
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


--returns number of nodes where node A points to
iPointOn :: Eq a => [(a, b)] -> a -> Int
iPointOn f find = length [ x | (x,_) <- f, x == find]

--returns number of nodes pointing on A
pointingOnMe :: Eq b => [(a, b)] -> b -> Int
pointingOnMe f find = length [ x | (_,x) <- f, x == find]


projectFunc :: IO ()
projectFunc = do
  file <- openFile "data.jl" ReadMode
  {-jsonLine <- hGetLine file
  let lineBS = strToBS jsonLine
  let mm = decode lineBS :: Maybe Page
  case mm of
    Nothing -> print "error parsing JSON"
    Just m -> do
      --links <- printUrl m
      --print links
      bodyText <- printBody m
      print bodyText
  -}
  let link =[[]]
  --let linksTest = [(, )]
  links<-readLineByLine file link
  --linksT<-readLineByLine file linksTest
  print (links)

  let test = iPointOn [("sa","fae"),("sa","fgrs"),("gs","tt")] "sa"

  print(test)

  hClose file
