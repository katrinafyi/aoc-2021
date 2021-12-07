module Main where

import Numeric
import Data.List
import Data.Char
import Data.Maybe
import Text.Read(readMaybe)
import Control.Applicative
import qualified Data.Map.Strict as M

import Lib


parse = ints

solve :: (Int -> Int) -> [Int] -> Int
solve f xs  = minimum $ (\pos -> sum $ (f . abs . (-) pos) <$> xs) <$> poss
    where
        poss = [minimum xs..maximum xs]

solve1 = solve id

tri n = (n+1) * n `div` 2
solve2 = solve tri

main' :: String -> IO ()
main' raw = do
    let input = parse raw
    print $ input

    print $ solve1 input
    print $ solve2 input

run :: IO ()
run = readFile "in/7.txt" >>= main'

test :: IO ()
test = readFile "in/7t.txt" >>= main'

main :: IO ()
main = getContents >>= main'

