module Lib where

import Data.List
import Data.Char
import Data.Maybe
import qualified Data.Map.Strict as M
import Text.Read

slidingParts :: Int -> [a] -> [[a]]
slidingParts _ [] = []
slidingParts n xs@(_:xs') = take n xs : sliding n xs'

sliding :: Int -> [a] -> [[a]]
sliding n = filter ((== n) . length) . slidingParts n

chunks :: Int -> [a] -> [[a]]
chunks _ [] = []
chunks n xs = take n xs : chunks n (drop n xs)

replace :: Eq a => a -> a -> [a] -> [a]
replace a b = fmap (\c -> if c == a then b else c)

count :: (a -> Bool) -> [a] -> Int
count f xs = length $ filter f xs

ints :: String -> [Int]
ints s = catMaybes $ readMaybe <$> words digits
    where
        digits = fmap (\c -> if isDigit c || c == '-' then c else ' ') s

increment :: (Ord a, Integral b) => a -> M.Map a b -> M.Map a b
increment k = M.insertWith (+) k 1

modifyNth :: Int -> (a -> a) -> [a] -> [a]
modifyNth n f xs = front ++ fmap f backHead ++ backTail
    where
        (front, back) = splitAt n xs
        (backHead, backTail) = splitAt 1 back

-- median :: (Num a, Fractional b) => [a] -> b
-- median xs
--     | len `mod` 2 == 1 = fromIntegral $ xs !! mid
--     | otherwise = (fromIntegral (xs !! (mid-1)) + fromIntegral (xs !! mid)) / 2
--     where
--         len = length xs
--         mid = len `div` 2

average :: (Real a, Fractional b) => [a] -> b
average xs = realToFrac (sum xs) / genericLength xs

filterByFreq :: Ord a => Int -> [a] -> [a]
filterByFreq n = concatMap (take 1) . filter ((== n) . length) . group . sort

one :: [a] -> a
one [x] = x
one _ = error "list does not contain exactly one element"

zipMap :: (a -> b) -> [a] -> [(a, b)]
zipMap f = zip <$> id <*> fmap f
