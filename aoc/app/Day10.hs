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

data Side = L | R deriving (Eq, Show)
data Type = Round | Square | Angle | Curly deriving (Eq, Show)
data Bracket = B { bSide :: Side, bType :: Type } deriving (Eq, Show)

parseB :: Char -> Maybe Bracket
parseB c = case c of
    '(' -> Just $ B L Round
    ')' -> Just $ B R Round
    '[' -> Just $ B L Square
    ']' -> Just $ B R Square
    '{' -> Just $ B L Curly
    '}' -> Just $ B R Curly
    '<' -> Just $ B L Angle
    '>' -> Just $ B R Angle
    _ -> Nothing

parseLine :: [Char] -> [Bracket]
parseLine = mapMaybe parseB

parse :: String -> [[Bracket]]
parse = fmap parseLine . lines

isPair :: Bracket -> Bracket -> Bool
isPair (B L t1) (B R t2) = t1 == t2
isPair _ _ = False

mismatched :: [Bracket] -> [Bracket]
mismatched = go []
    where
        -- stack of opened brackets; bracket sequence
        -- returns mismatched brackets
        go :: [Bracket] -> [Bracket] -> [Bracket]
        go ls [] = ls
        go ls (b@(B L _):bs) = go (b:ls) bs
        go (l:ls) (b:bs)
            | isPair l b = go ls bs
        go ls (b:bs) = b : go ls bs

score :: Type -> Int
score Round = 3
score Square = 57
score Curly = 1197
score Angle = 25137

isCorrupt :: [Bracket] -> Bool
isCorrupt bs = hasL && hasR
    where
        hasL = any ((== L) . bSide) mismatch
        hasR = any ((== R) . bSide) mismatch
        mismatch = mismatched bs

solveLine :: [Bracket] -> Int
solveLine bs
    | isCorrupt bs = score $ bType $ head $ mismatched bs
    | otherwise = 0

solve1 :: [[Bracket]] -> Int
solve1 = sum . fmap solveLine

score2 :: Type -> Int
score2 Round = 1
score2 Square = 2
score2 Curly = 3
score2 Angle = 4

solveLine2 :: [Bracket] -> Int
solveLine2 = foldl1' (\a x -> 5*a + x) . fmap (score2 . bType) . mismatched

solve2 :: [[Bracket]] -> Int
solve2 bs = nums !! (length nums `div` 2)
    where nums = sort $ solveLine2 <$> filter (not . isCorrupt) bs

main' :: String -> IO ()
main' raw = do
    let input = parse raw
    -- print $ input

    print $ mismatched $ head input


    print $ solve1 input
    print $ solve2 input

run :: IO ()
run = readFile "in/10.txt" >>= main'

test :: IO ()
test = readFile "in/10t.txt" >>= main'

main :: IO ()
main = getContents >>= main'

