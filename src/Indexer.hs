{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Indexer where
import qualified Data.Map as Map
import Data.List
import Data.Ord

insertMap :: [String] -> String -> Map.Map String [String] -> Map.Map String [String]
insertMap [] _ map = map
insertMap (x:xs) y map = insertMap xs y (Map.insertWith (++) x [y] map)

--create csv file and write Map to it. Format is: key, values
writeMap :: Map.Map String [String] -> IO ()
writeMap map = writeFile "index.csv" (showMap map)
  where
    showMap :: Map.Map String [String] -> String
    showMap map = foldl (\acc x -> acc ++ show x ++ "\n") "" (Map.toList map)

--source: https://stackoverflow.com/questions/26260752/sort-one-list-by-the-order-of-another-list
sortAlong :: Eq b => [b] -> [b] -> [b]
sortAlong order = map snd . sortBy (comparing fst) . map (\x -> (lookup x z, x))
    where
    z = zip order [0..]

sortMap :: Map.Map String [String] -> [String] -> Map.Map String [String]
sortMap map pageRank = Map.map (sortAlong pageRank) map