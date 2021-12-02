module Main where

import Data.Bifunctor
import Data.List
import Data.Maybe
import Text.Read(readMaybe)

import Lib

liftTuple :: (a -> b) -> (a, a) -> (b, b)
liftTuple f (x, y) = (f x, f y)

step :: ((Int, Int), Int) -> [Int]
step (d, n) = [n * fst d, n * snd d]

cumsum :: [Int] -> [Int]
cumsum xs = go 0 xs
    where
    go n [] = []
    go n (x:xs) = (x+n):go (x+n) xs

--solve :: [((Int, Int), Int)] -> (Int, Int)
solve xs = (horizontal, depth)
    where 
    [as, ms] = transpose . fmap step $ xs 
    aims = cumsum as
    horizontal = sum ms
    depth = sum $ zipWith (\a m -> a*m) aims ms

solve1 = solve
solve2 = undefined

parseD "forward" = (0, 1)
parseD "backward" = (0, -1)
parseD "up" = (-1, 0)
parseD "down" = (1, 0)

parseLine :: String -> Maybe ((Int, Int), Int)
parseLine l = do 
    let d = parseD $ takeWhile (/= ' ') l
    n <- readMaybe $ tail $ dropWhile (/= ' ') l
    pure (d, n)

parse = catMaybes . fmap parseLine . lines

main :: IO ()
main = do
    input <- parse <$> getContents
    print $ solve1 input
    --print $ solve2 input

