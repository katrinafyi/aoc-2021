module Main where

import Numeric
import Data.List
import Data.Char
import Data.Maybe
import Text.Read(readMaybe)
import Control.Applicative
import qualified Data.Map.Strict as M
import qualified Data.Set as S

import Lib
import Data.Function
import Control.Monad (guard)



parseLine :: [Char] -> [Int]
parseLine = fmap (read . pure)

adjacents :: (Int,Int) -> [(Int,Int)]
adjacents (r,c) = [(r+1,c), (r-1,c), (r,c+1), (r,c-1)]

parse :: String -> M.Map (Int,Int) Int
parse raw = M.fromList $ do
        (r, cols) <- rows'
        (c, x) <- zip [0..] cols
        pure ((r, c), x)
    where
        rows' = zip [0..] $ fmap parseLine rows
        rows = lines raw

localMin :: M.Map (Int,Int) Int -> (Int,Int) -> Bool
localMin m pos = all (> x) adjs
    where
        adjs = mapMaybe (\a -> M.lookup a m) $ adjacents pos
        x = fromJust $ M.lookup pos m

solve1 :: M.Map (Int,Int) Int -> Int
solve1 m = sum $ (+1) <$> M.filterWithKey (const . localMin m) m

basin :: M.Map (Int,Int) Int -> (Int,Int) -> [(Int,Int)]
basin m bot = go [] [bot]
    where
        go :: [(Int,Int)] -> [(Int,Int)] -> [(Int,Int)]
        go seen [] = seen
        go seen (x:xs) = do
            let val = m M.! x
            let seen' = x:seen
            let adjs = filter (`M.member` m) $ adjacents x \\ seen'
            let isHigher v' = v' > val && v' < 9
            if all (isHigher . (m M.!)) adjs
                then go (x:seen) (xs++adjs)
                else go seen xs
        getVal x = m M.!? x

solve2 :: M.Map (Int,Int) Int -> Int
solve2 m = product $ length <$> basin m <$> mins
    where
        mins = M.keys $ M.filterWithKey (const . localMin m) m


main' :: String -> IO ()
main' raw = do
    let input = parse raw
    print $ input

    print $ solve1 input
    print $ solve2 input

run :: IO ()
run = readFile "in/9.txt" >>= main'

test :: IO ()
test = readFile "in/9t.txt" >>= main'

main :: IO ()
main = getContents >>= main'

