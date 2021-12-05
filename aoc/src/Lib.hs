module Lib where

import Data.Char
import Data.Maybe
import qualified Data.Map.Strict as M
import Text.Read

slidingParts :: Int -> [a] -> [[a]]
slidingParts _ [] = []
slidingParts n xs@(_:xs') = take n xs : sliding n xs'

sliding :: Int -> [a] -> [[a]]
sliding n = filter ((== n) . length) . slidingParts n

chunks :: Int -> [a] -> [[a]]
chunks _ [] = []
chunks n xs = take n xs : chunks n (drop n xs)

replace :: Eq a => a -> a -> [a] -> [a]
replace a b = fmap (\c -> if c == a then b else c)

count :: (a -> Bool) -> [a] -> Int
count f xs = length $ filter f xs

ints :: String -> [Int]
ints s = catMaybes $ readMaybe <$> words digits
    where
        digits = fmap (\c -> if isDigit c || c == '-' then c else ' ') s

increment :: (Ord a, Integral b) => a -> M.Map a b -> M.Map a b
increment k = M.insertWith (+) k 1
