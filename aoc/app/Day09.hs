module Main where

import Numeric
import Data.List
import Data.Char
import Data.Maybe
import Text.Read(readMaybe)
import Control.Applicative
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Sequence as SS

import Lib
import Data.Function
import Control.Monad (guard)



parseLine :: [Char] -> [Int]
parseLine = fmap (read . pure)

parse :: String -> M.Map (Int,Int) Int
parse raw = M.fromList $ do
        (r, cols) <- rows'
        (c, x) <- zip [0..] cols
        pure ((r, c), x)
    where
        rows' = zip [0..] $ fmap parseLine rows
        rows = lines raw

localMin :: M.Map (Int,Int) Int -> (Int,Int) -> Bool
localMin m pos = x < 9 && all (>= x) adjs
    where
        adjs = mapMaybe (\a -> M.lookup a m) $ adjacents pos
        x = fromMaybe 9 $ M.lookup pos m

solve1 :: M.Map (Int,Int) Int -> Int
solve1 m = sum $ (+1) <$> M.filterWithKey (const . localMin m) m

basin :: M.Map (Int,Int) Int -> (Int,Int) -> [(Int,Int)]
basin map bot = go map [bot]
    where
        go :: M.Map (Int,Int) Int -> [(Int,Int)] -> [(Int,Int)]
        go m [] = M.keys $ M.difference map m
        go m (x:xs)
            | val < 9 = go m' (xs ++ adjs)
            | otherwise = go m xs
            where
                adjs = filter (`M.member` m) $ adjacents x
                m' = M.delete x m
                val = fromMaybe 10 $ m M.!? x

-- solve2 :: M.Map (Int,Int) Int -> Int
solve2 m = product $ take 3 $ reverse $ sort $ length <$> basin m <$> mins
    where
        mins = M.keys $ M.filterWithKey (const . localMin m) m


main' :: String -> IO ()
main' raw = do
    let input = parse raw
    -- print $ input

    print $ solve1 input
    print $ solve2 input

run :: IO ()
run = readFile "in/9.txt" >>= main'

test :: IO ()
test = readFile "in/9t.txt" >>= main'

main :: IO ()
main = getContents >>= main'

