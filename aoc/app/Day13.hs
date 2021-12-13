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

parse :: String -> (S.Set (Int,Int), [Fold])
parse raw = (dots, parseFold <$> tail fs)
    where
        (ds, fs) = break null $ lines raw
        dots = S.fromList $ two . ints <$> ds

doFoldY :: Int -> S.Set (Int,Int) -> S.Set (Int,Int)
doFoldY n b = top <> bot'
    where
        (top, bot) = S.partition ((<n) . snd) b
        bot' = S.map (second $ \y -> n - abs (y-n)) bot

doFoldX :: Int -> S.Set (Int,Int) -> S.Set (Int,Int)
doFoldX n = S.map swap . doFoldY n . S.map swap

doFold :: Fold -> S.Set (Int,Int) -> S.Set (Int,Int)
doFold (FoldX n) = doFoldX n
doFold (FoldY n) = doFoldY n

solve1 :: (S.Set (Int,Int), [Fold]) -> Int
solve1 (b, f:_) = length $ doFold f b
solve1 (_, _) = undefined

solve2 :: (S.Set (Int,Int), [Fold]) -> S.Set (Int,Int)
solve2 (b, fs) = foldl' (flip doFold) b fs

main' :: String -> IO ()
main' raw = do
    let input = parse raw
    -- print $ input

    print $ solve1 input
    let s2 = solve2 input
    print $ s2
    let maxX = maximum $ S.map fst s2
    let maxY = maximum $ S.map snd s2
    let ls = chunks (maxX+1) $ bool ' ' '#' . (`S.member` s2) . swap <$> cartesian2 [0..maxY] [0..maxX]
    mapM_ putStrLn ls


run :: IO ()
run = readFile "in/13.txt" >>= main'

test :: IO ()
test = readFile "in/13t.txt" >>= main'

main :: IO ()
main = getContents >>= main'

