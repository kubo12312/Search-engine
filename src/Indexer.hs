{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Indexer where
import qualified Data.Map as Map

insertMap :: [String] -> String -> Map.Map String [String] -> Map.Map String [String]
insertMap [] _ map = map
insertMap (x:xs) y map = insertMap xs y (Map.insertWith (++) x [y] map)
