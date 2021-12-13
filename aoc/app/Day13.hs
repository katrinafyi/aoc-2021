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
import Data.Bifunctor
import Data.Tuple
import Data.Bool

data Fold = FoldX Int | FoldY Int deriving (Eq, Show)

parseFold :: String -> Fold
parseFold s
    | 'x' `elem` s = FoldX n
    | otherwise = FoldY n
    where [n] = ints s

parse :: String -> ([[Bool]], [Fold])
parse raw = (board, parseFold <$> tail fs)
    where
        (ds, fs) = break null $ lines raw
        dots = two . ints <$> ds
        maxX = maximum $ fmap fst dots
        maxY = maximum $ fmap snd dots
        board = (\y -> (\x -> (x,y) `elem` dots) <$> [0..maxX]) <$> [0..maxY]

doFoldY :: Int -> [[Bool]] -> [[Bool]]
doFoldY n b = zipWith (zipWith (||)) top (reverse bot)
    where
        top = take n b
        bot = drop (n+1) b

doFoldX :: Int -> [[Bool]] -> [[Bool]]
doFoldX n b = transpose $ doFoldY n $ transpose b

doFold :: Fold -> [[Bool]] -> [[Bool]]
doFold (FoldX n) = doFoldX n
doFold (FoldY n) = doFoldY n

solve1 :: ([[Bool]], [Fold]) -> Int
solve1 (b, f:_) = count id $ concat $ doFold f b

solve2 :: ([[Bool]], [Fold]) -> [[Bool]]
solve2 (b, fs) = foldl' (\b f -> doFold f b) b fs

main' :: String -> IO ()
main' raw = do
    let input = parse raw
    -- print $ input
    -- mapM_ putStrLn $ intercalate "," <$> allPaths2 input

    print $ solve1 input
    let s2 = solve2 input
    mapM_ putStrLn $ fmap (bool ' ' '#') <$> s2

run :: IO ()
run = readFile "in/13.txt" >>= main'

test :: IO ()
test = readFile "in/13t.txt" >>= main'

main :: IO ()
main = getContents >>= main'

