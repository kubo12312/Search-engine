module Search where
import GHC.OldList (isInfixOf)
import Data.List.Split

readCsv :: IO [String]
readCsv = do
  file <- readFile "index.csv"
  let alllines = lines file
  return alllines

search :: String -> [String] -> IO [String]
search word words = do
  let result = filter (\x -> splitOn "," x !! 0 == word) words
  let resultSplit = splitArray result
  return (reverse (firstLast resultSplit))

splitArray :: [String] -> [String]
splitArray [] = []
splitArray (x:xs) = splitOn "," x

firstLast::[a]->[a]
firstLast [] = []
firstLast [x] = []
firstLast xs = tail (init xs)