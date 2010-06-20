module Matrices where

import List

showMatrix :: [[Integer]] -> String
showMatrix m = unlines $ map (layoutRow $ measureColumns sm) sm
    where sm = stringizeMatrix m
          stringizeMatrix :: [[Integer]] -> [[String]]
          stringizeMatrix = map (map show)
          measureColumns :: [[String]] -> [Int]
          measureColumns = foldr1 (zipWith max) . map (map length)
          layoutRow :: [Int] -> [String] -> String
          layoutRow widths row = "[" ++ (concat $ intersperse " " $ zipWith justify widths row) ++ "]"
          justify :: Int -> String -> String
          justify n s = replicate (n - length s) ' ' ++ s
