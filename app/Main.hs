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
import Text.HandsomeSoup
import Text.Regex.TDFA
import Text.XML.HXT.Core

data Page = Page
  { url :: String,
    html_content :: String
  }
  deriving (Generic)

instance FromJSON Page

strToBS :: String -> L.ByteString
strToBS = L.packChars

printUrl :: Page -> IO ()
printUrl m =
  do
    let doc = readString [withParseHTML yes, withWarnings no] (html_content m)
    links <- runX $ doc >>> css "a" ! "href"
    mapM_ putStrLn links

readLineByLine :: Handle -> IO ()
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

main = do
  file <- openFile "collection.jl" ReadMode
  jsonLine <- hGetLine file
  let lineBS = strToBS jsonLine
  let mm = decode lineBS :: Maybe Page
  case mm of
    Nothing -> print "error parsing JSON"
    Just m -> printUrl m
  --readLineByLine file
  hClose file