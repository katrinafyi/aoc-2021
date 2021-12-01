module Lib where

slidingParts :: Int -> [a] -> [[a]]
slidingParts _ [] = []
slidingParts n xs@(_:xs') = take n xs : sliding n xs'

sliding :: Int -> [a] -> [[a]]
sliding n = filter ((== n) . length) . slidingParts n

chunks :: Int -> [a] -> [[a]]
chunks _ [] = []
chunks n xs = take n xs : chunks n (drop n xs)
