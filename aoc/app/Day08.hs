module Main where

import Numeric
import Data.List
import Data.Char
import Data.Maybe
import Text.Read(readMaybe)
import Control.Applicative
import qualified Data.Map.Strict as M

import Lib
import Data.Function
import Control.Monad (guard)

newtype Out = Out Char deriving (Eq, Show, Ord)
newtype In = In Char deriving (Eq, Show, Ord)
data Segment = Segment { digitSignals :: [[In]], outputSignals :: [[In]] } deriving (Eq, Show)


parseLine :: [Char] -> Segment
parseLine xs = Segment (fmap In <$> take 10 ws) (fmap In <$> drop 11 ws)
    where
        ws = words xs

parse :: String -> [Segment]
parse = fmap parseLine . lines


digitSegs :: Int -> [Out]
digitSegs 0 = fmap Out "abcefg"
digitSegs 1 = fmap Out "cf"
digitSegs 2 = fmap Out "acdeg"
digitSegs 3 = fmap Out "acdfg"
digitSegs 4 = fmap Out "bcdf"
digitSegs 5 = fmap Out "abdfg"
digitSegs 6 = fmap Out "abdefg"
digitSegs 7 = fmap Out "acf"
digitSegs 8 = fmap Out "abcdefg"
digitSegs 9 = fmap Out "abcdfg"


feasible :: [Maybe Out] -> [Out] -> Bool
feasible ms os =
    length os == length ms && sort (catMaybes ms) `isSubsequenceOf` os

outSigs = digitSegs <$> [1, 7, 4, 8, 2, 3, 5, 6, 9, 0]

verify :: [[In]] -> (In -> Maybe Out) -> Bool
verify sigs f = go outSigs
    where all (\s -> any (feasible (fmap f s)) outSigs) sigs

showFun :: (In -> Out) -> [(In, Out)]
showFun f = (\x -> (x, f x)) <$> fmap In ['a'..'g']

solveDigits :: [[In]] -> In -> Out
solveDigits sigs = fromJust . head sol
    where
        sol = go (fmap In ['a'..'g']) (fmap Out ['a'..'g']) (const Nothing)

        go :: [In] -> [Out] -> (In -> Maybe Out) -> [In -> Maybe Out]
        go [] [] f = pure f
        go ins outs f = do
            i <- ins
            o <- outs
            let f' = \x -> if x == i then Just o else f x
            guard $ verify (filter (i `elem`) sigs) f'
            go (delete i ins) (delete o outs) f'

mapZip :: (a -> b) -> [a] -> [(a, b)]
mapZip f xs = zip xs (fmap f xs)

sigToDigit :: [Out] -> Int
sigToDigit sig = fst $ head $ filter ((== sig') . snd) $ mapZip digitSegs [0..9]
    where sig' = sort sig


solveSeg :: Segment -> [Int]
solveSeg (Segment sigs outSigs) = fmap sigToDigit outs
    where
        f = solveDigits sigs
        outs = fmap f <$> outSigs


solve1 :: [Segment] -> Int
solve1 = count (`elem` [2,7,4,3]) . fmap length . concatMap outputSignals

solve2 :: [Segment] -> Int
solve2 = sum . fmap (foldl1' (\a x -> 10*a+x) . solveSeg)

main' :: String -> IO ()
main' raw = do
    let input = parse raw
    -- print $ input

    print $ solve1 input
    print $ solve2 input

run :: IO ()
run = readFile "in/8.txt" >>= main'

test :: IO ()
test = readFile "in/8t.txt" >>= main'

main :: IO ()
main = getContents >>= main'

