module Main where

import Numeric
import Data.List
import Data.Char
import Data.Maybe
import Text.Read(readMaybe)
import Control.Applicative
import qualified Data.Map.Strict as M

import Lib


step :: [Int] -> [Int]
step (x:xs) = modifyNth 6 (+x) $ xs ++ [x]

parse :: String -> [Int]
parse raw = fmap (\n -> count (== n) nums) [0..8]
    where nums = ints raw

solve1 xs = iterate step xs !! 80
solve2 xs = iterate step xs !! 256

main :: IO ()
main = do
    input <- parse <$> getContents
    print $ input

    print $ sum $ solve1 input
    print $ sum $ solve2 input

