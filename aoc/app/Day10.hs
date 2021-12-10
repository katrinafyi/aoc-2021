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
data Bracket = Bracket { bSide :: Side, bType :: Type } deriving (Eq, Show)

parseB :: Char -> Maybe Bracket
parseB c = case c of
    '(' -> Just $ Bracket L Round
    ')' -> Just $ Bracket R Round
    '[' -> Just $ Bracket L Square
    ']' -> Just $ Bracket R Square
    '{' -> Just $ Bracket L Curly
    '}' -> Just $ Bracket R Curly
    '<' -> Just $ Bracket L Angle
    '>' -> Just $ Bracket R Angle
    _ -> Nothing

parseLine :: [Char] -> [Bracket]
parseLine = mapMaybe parseB

parse :: String -> [[Bracket]]
parse = fmap parseLine . lines

mismatched :: [Bracket] -> [Bracket]
mismatched = go []
    where
        -- stack of type of opened brackets; bracket sequence
        -- returns mismatched brackets
        go :: [Type] -> [Bracket] -> [Bracket]
        go ls [] = Bracket L <$> ls
        go ls (b@(Bracket L t):bs) = go (t:ls) bs
        go [] (b@(Bracket R _):bs) = b : go [] bs
        go (l:ls) (b@(Bracket R t):bs)
            | l == t = go ls bs
            | otherwise = b : go (l:ls) bs

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
solveLine2 bs = foldl1' (\a x -> 5*a + x) $ fmap (score2 . bType) mis
    where
        mis = mismatched bs

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

