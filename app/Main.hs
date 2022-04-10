{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

import Data.Aeson
import Data.ByteString as S (ByteString)
import Data.ByteString.Char8 as C8 (pack)
import Data.ByteString.Lazy.Internal as L
import Data.List
import qualified Data.Text as T
import GHC.Generics
import System.IO
import Text.HTML.Parser
import Text.HTML.Scalpel
import Text.HandsomeSoup
import Text.Regex.TDFA
import Text.XML.HXT.Core
import Zenacy.HTML

data Page = Page
  { url :: String,
    html_content :: String
  }
  deriving (Generic)

instance FromJSON Page

cleanUrl :: String -> [String] -> [[String]]
cleanUrl url x = [[url, x] | x <- x, "http" `isPrefixOf` x]

strToBS :: String -> L.ByteString
strToBS = L.packChars

printUrl :: Page -> IO [[String]]
printUrl m =
  do
    --putStrLn $ url m
    let doc = readString [withParseHTML yes, withWarnings no] (html_content m)
    links <- runX $ doc >>> css "a" ! "href"

    --let body = scrapeStringLike (html_content m) (texts "body")
    --print body

    let linksClean = cleanUrl (url m) links
    return linksClean
    --print linksClean

printBody :: Page -> IO ()
printBody m =
  do
    let doc = readString [withParseHTML yes, withWarnings no] (html_content m)
    body <- runX $ doc >>> css "body" //> neg (css "script") >>> removeAllWhiteSpace //> getText
    print body

{-readLineByLine :: Handle -> IO ()
readLineByLine input =
  do
    line <- hIsEOF input
    if line
      then return ()
      else do
        jsonLine <- hGetLine input
        let lineBS = strToBS jsonLine
        let mm = decode lineBS :: Maybe Page
        case mm of
          Nothing -> print "error parsing JSON"
          Just m -> printUrl m
        readLineByLine input
-}

main = do
  file <- openFile "collection.jl" ReadMode
  jsonLine <- hGetLine file
  let lineBS = strToBS jsonLine
  let mm = decode lineBS :: Maybe Page
  case mm of
    Nothing -> print "error parsing JSON"
    Just m -> do
      --links <- printUrl m
      --print links
      printBody m      
      
  --readLineByLine file
  hClose file
