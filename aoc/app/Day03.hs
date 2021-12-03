module Main where

import Numeric
import Data.List
import Data.Char
import Data.Maybe
import Text.Read(readMaybe)
import Control.Applicative

import Lib

binToInt :: String -> Maybe Int
binToInt = fmap fst . listToMaybe . readInt 2 (`elem` "01") digitToInt

count :: (a -> Bool) -> [a] -> Int
count f xs = length $ filter f xs

flip' :: Char -> Char
flip' '1' = '0'
flip' '0' = '1'

-- Converts list of list of bools to a  list of orderings, where each
-- ordering is the number of True compared to the number of False.
orderings :: [[Bool]] -> [Ordering]
orderings xs = (\c -> c `compare` (len-c)) <$> counts
    where
        counts = count (== True) <$> transpose xs
        len = length xs

solve :: [String] -> (String, String)
solve nums = (ordToNum <$> ords, flip' . ordToNum <$> ords)
    where
        ords = orderings $ fmap (== '1') <$> nums
        ordToNum GT = '1'
        ordToNum EQ = undefined
        ordToNum LT = '0'


o2 GT = '1'
o2 EQ = '1'
o2 LT = '0'

co2 GT = '0'
co2 EQ = '0'
co2 LT = '1'

filter2 :: (Ordering -> Char) -> [String] -> [String]
filter2 _ [] = []
filter2 _ [n] = [n]
filter2 f nums = fmap (first ++) $ filter2 f $ fmap tail $ filter (\n -> take 1 n == first) nums
    where
        ords = orderings $ fmap (== '1') <$> nums
        keep = f <$> ords
        first = take 1 keep

solve1 = solve
solve2 :: [String] -> (String, String)
solve2 nums = (head $ filter2 o2 nums, head $ filter2 co2 nums)


parseLine :: String -> Maybe String
parseLine x
    | length x > 0 = Just x
    | otherwise = Nothing

parse = catMaybes . fmap parseLine . lines

main :: IO ()
main = do
    input <- parse <$> getContents
    let (g, e) = solve1 input
    print $ liftA2 (*) (binToInt g) (binToInt e)
    let (a, b) = solve2 input
    print $ (a,b)
    print $ liftA2 (*) (binToInt a) (binToInt b)

