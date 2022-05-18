
module Search where
import GHC.OldList (isInfixOf)
import Data.List.Split

readCsv :: IO [String]
readCsv = do
  file <- readFile "index.csv"
  let alllines = lines file
  return alllines

findDuplicates :: [String] -> [String] -> [String]
findDuplicates [] _ = []
findDuplicates _ [] = []
findDuplicates (x:xs) ys
  | elem x ys = x : findDuplicates xs ys
  | otherwise = findDuplicates xs ys

search :: String -> [String] -> [String]
search word words = do
  let result = filter (\x -> head (splitOn "," x) == word) words
  let resultSplit = splitArray result
  firstLast resultSplit

searchHandle :: [String] -> [String] -> [String] -> [String]
searchHandle input wordInCsv result = do
    if null input
        then result
        else do
            let word = head input
            let newResult = search word wordInCsv
            if null result
                then searchHandle (tail input) wordInCsv newResult
            else if null newResult
                then searchHandle (tail input) wordInCsv result
            else do
              let duplicates = findDuplicates newResult result
              searchHandle (tail input) wordInCsv duplicates


splitArray :: [String] -> [String]
splitArray [] = []
splitArray (x:xs) = splitOn "," x

firstLast::[a]->[a]
firstLast [] = []
firstLast [x] = []
firstLast xs = tail (init xs)