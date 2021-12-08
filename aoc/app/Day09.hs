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
localMin m pos = x < 9 && all (> x) adjs
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
            | localMin m x = go m' $ sortOn (m M.!?) $ xs ++ adjs
            | otherwise = go m xs
            where
                adjs = filter (`M.member` m) $ adjacents x
                m' = M.delete x m

ins :: M.Map (Int, Int) Int
ins = M.fromList [((0,0),2),((0,1),1),((0,2),9),((0,3),9),((0,4),9),((0,5),4),((0,6),3),((0,7),2),((0,8),1),((0,9),0),((1,0),3),((1,1),9),((1,2),8),((1,3),7),((1,4),8),((1,5),9),((1,6),4),((1,7),9),((1,8),2),((1,9),1),((2,0),9),((2,1),8),((2,2),5),((2,3),6),((2,4),7),((2,5),8),((2,6),9),((2,7),8),((2,8),9),((2,9),2),((3,0),8),((3,1),7),((3,2),6),((3,3),7),((3,4),8),((3,5),9),((3,6),6),((3,7),7),((3,8),8),((3,9),9),((4,0),9),((4,1),8),((4,2),9),((4,3),9),((4,4),9),((4,5),6),((4,6),5),((4,7),6),((4,8),7),((4,9),8)]

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

