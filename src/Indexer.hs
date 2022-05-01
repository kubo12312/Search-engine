module Indexer where
import qualified Data.Map as Map

insertMap :: [String] -> String -> Map.Map String [String] -> Map.Map String [String]
insertMap [] _ map = map
insertMap (x:xs) y map = insertMap xs y (Map.insertWith (++) x [y] map)

--sort and array of strings in Map (Map String [String]) by order in the another array of strings
sortMap :: [String] -> Map.Map String [String] -> Map.Map String [String]
sortMap [] map = map
sortMap (x:xs) map = sortMap xs (Map.insertWith (++) x xs map)

--create csv file and write Map to it. Format is: key, values
writeMap :: Map.Map String [String] -> IO ()
writeMap map = writeFile "index.csv" (showMap map)
  where
    showMap :: Map.Map String [String] -> String
    showMap map = foldl (\acc x -> acc ++ show x ++ "\n") "" (Map.toList map)