module Main where

import Numeric
import Data.List
import Data.Char
import Data.Maybe
import Text.Read(readMaybe)
import Control.Applicative

import Lib hiding (replace)

binToInt :: String -> Maybe Int
binToInt = fmap fst . listToMaybe . readInt 2 (`elem` "01") digitToInt

count :: (a -> Bool) -> [a] -> Int
count f xs = length $ filter f xs


replace :: Eq a => a -> a -> [a] -> [a]
replace old new = fmap (\c -> if c == old then new else c)

parseLine :: String -> Maybe String
parseLine x
    | length x > 0 = Just x
    | otherwise = Nothing

takeWhileRest :: (a -> Bool) -> [a] -> ([a], [a])
takeWhileRest f xs = (takeWhile f xs, dropWhile f xs)

data Cell = Cell { cellNum :: Int, cellState :: Bool } deriving (Eq, Show)
type Board = [[Cell]]
data State = State { nums :: [Int], boards :: [Board] } deriving (Eq, Show)

setCell :: Bool -> Cell -> Cell
setCell b (Cell n _) = Cell n b

markCell :: Int -> Cell -> Cell
markCell n c@(Cell n2 _)
    | n == n2 = setCell True c
    | otherwise = c

markBoard :: Int -> Board -> Board
markBoard n b = fmap (markCell n) <$> b

markNext :: State -> State
markNext (State (n:ns) bs) = State ns (markBoard n <$> bs)

cellWin (Cell _ b) = b

boardWin :: Board -> Bool
boardWin bs = (or $ fmap and marked) || (or $ fmap and (transpose marked))
    where
        marked :: [[Bool]]
        marked = fmap cellWin <$> bs

parseBoards :: [String] -> [Board]
parseBoards [] = []
parseBoards lines = fmap (fmap (flip Cell False . read) . words) firstLines : parseBoards rest
    where
        (firstLines, rest) = takeWhileRest (/= "") $ dropWhile (== "") lines

parse raw = State nums boards
    where
        allLines = lines raw
        nums = fmap read $ words $ replace ',' ' ' $ head allLines
        boards = parseBoards $ drop 2 allLines



solve :: State -> (Int, Int, Board)
solve s@(State (n:_) bs)
    | not (null winning) = (-1, winningSum, head winning)
    | otherwise = (if a == -1 then n else a ,b,c)
    where
        (a,b,c) = solve $ markNext s
        winning = filter boardWin bs
        winningSum = sum $ fmap cellNum $ filter (not . cellWin) $ concat $ head winning

solve1 = solve

solve2 :: State -> (Int, Int, Board)
solve2 s@(State (n:_) bs)
    | not (null winning) && length winning == length bs = (-1, 0, last winning)
    | a == -1 = (n, winningSum $ markBoard n (head nonWinning),c)
    | otherwise = (a,b,c)
    where
        (a,b,c) = solve2 $ markNext s
        winning = filter boardWin bs
        nonWinning = filter (not . boardWin) bs
        winningSum b = sum $ fmap cellNum $ filter (not . cellWin) $ concat $ b

main :: IO ()
main = do
    input <- parse <$> getContents
    print $ input
    print $ solve1 input
    print $ solve2 input
    -- print $ solve1 input
    -- print $ solve2 input

