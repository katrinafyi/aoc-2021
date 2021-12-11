module Lib where

import Data.List
import Data.Char
import Data.Maybe
import qualified Data.Map.Strict as M
import Text.Read
import Data.Bifunctor
import Data.Tuple
import Control.Applicative

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

adjacents :: (Int,Int) -> [(Int,Int)]
adjacents (r,c) = [(r+1,c), (r-1,c), (r,c+1), (r,c-1)]

makeGrid :: [[a]] -> [((Int,Int),a)]
makeGrid rows = do
    (r, cols) <- zip [0..] rows
    (c, x) <- zip [0..] cols
    pure ((r, c), x)

makeGridXY :: [[c]] -> [((Int, Int), c)]
makeGridXY = fmap (first swap) . makeGrid

both :: (a -> b) -> (a, a) -> (b, b)
both = bimap <$> id <*> id

both2 :: (a -> a -> a) -> (a, a) -> (a, a) -> (a, a)
both2 f (x,y) = bimap (f x) (f y)

cartesian2 :: [a] -> [b] -> [(a,b)]
cartesian2 = liftA2 (,)

two :: [a] -> (a,a)
two [x,y] = (x,y)
two _ = error "two: list does not contain exactly two elements"
