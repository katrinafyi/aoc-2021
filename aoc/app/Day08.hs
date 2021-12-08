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


parseLine :: [Char] -> Segment
parseLine xs = Segment (take 10 ws) (drop 11 ws)
    where
        ws = fmap In <$> words xs

parse :: String -> [Segment]
parse = fmap parseLine . lines

newtype Out = Out Char deriving (Eq, Show, Ord)
newtype In = In Char deriving (Eq, Show, Ord)
data Segment = Segment { digitSignals :: [[In]], outputSignals :: [[In]] } deriving (Eq, Show)

-- a "signal" (or "sig") is a list of letters which are activated together

digitSigs :: Int -> [Out]
digitSigs 0 = fmap Out "abcefg"
digitSigs 1 = fmap Out "cf"
digitSigs 2 = fmap Out "acdeg"
digitSigs 3 = fmap Out "acdfg"
digitSigs 4 = fmap Out "bcdf"
digitSigs 5 = fmap Out "abdfg"
digitSigs 6 = fmap Out "abdefg"
digitSigs 7 = fmap Out "acf"
digitSigs 8 = fmap Out "abcdefg"
digitSigs 9 = fmap Out "abcdfg"
digitSigs _ = error "invalid digit"

verifySig :: [Maybe Out] -> [Out] -> Bool
verifySig ms os =
    length os == length ms && sort (catMaybes ms) `isSubsequenceOf` os

outSigs = digitSigs <$> [1, 7, 4, 8, 2, 3, 5, 6, 9, 0]

verifyMap :: [[In]] -> (In -> Maybe Out) -> Bool
verifyMap sigs f = all (\s -> any (verifySig (fmap f s)) outSigs) sigs

solveDigits :: [[In]] -> In -> Out
solveDigits sigs = fromJust . head sol
    where
        sol = go (In <$> ['a'..'g']) (Out <$> ['a'..'g']) (const Nothing)

        go :: [In] -> [Out] -> (In -> Maybe Out) -> [In -> Maybe Out]
        go [] [] f = pure f
        go ins outs f = do
            i <- ins
            o <- outs
            let f' = \x -> if x == i then Just o else f x
            guard $ verifyMap (filter (i `elem`) sigs) f'
            go (delete i ins) (delete o outs) f'


solveDigits' :: [[In]] -> In -> Out
solveDigits' sigs x = fromJust $ lookup x assocs
    where
        assocs = zip [a,b,c,d,e,f,g] (Out <$> ['a'..'g'])

        -- returns signals of length exactly n
        byLen n = filter ((== n) . length) sigs
        -- returns letters appearing n times across all signals
        byFreq n = filterByFreq n (concat sigs)

        -- signal lengths: 2, 3, 4, 5, 6, 7
        cf = one $ byLen 2 -- 1
        acf = one $ byLen 3 -- 7
        bcdf = one $ byLen 4 -- 4
        abcdefg = one $ byLen 7 -- 8

        a = one $ acf \\ cf
        bd = bcdf \\ cf

        adg = foldl1 intersect (byLen 5) -- 2,3,5
        abfg = foldl1 intersect (byLen 6) -- 0,6,9

        ag = adg `intersect` abfg

        g = one $ ag \\ [a]
        d = one $ adg \\ ag
        b = one $ bd \\ [d]
        f = one $ abfg `intersect` cf
        c = one $ cf \\ [f]

        e = one $ abcdefg \\ [a,b,c,d,f,g]

        -- byFreq: a 8, b 6, c 8, d 7, e 4, f 9, g 7
        -- b = one $ byFreq 6
        -- e = one $ byFreq 4
        -- f = one $ byFreq 9
        -- ac = byFreq 8
        -- dg = byFreq 7

        -- c = one $ cf \\ [f]
        -- d = one $ bcdf \\ [b,c,f]
        -- g = one $ dg \\ [d]


sigToDigit :: [Out] -> Int
sigToDigit sig = one $ filter ((== sig') . digitSigs) [0..9]
    where sig' = sort sig


solveSeg :: Segment -> [Int]
solveSeg (Segment sigs outSigs) = fmap sigToDigit outs
    where
        f = solveDigits' sigs
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

