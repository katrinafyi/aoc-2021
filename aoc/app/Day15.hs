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

type Input = M.Map (Int,Int) Int

parse :: [Char] -> Input
parse raw = M.fromList $ makeGridXY $ fmap (fmap (read . pure)) ls
    where ls = lines raw

type State = Input

step :: Input -> State -> (Int,Int) -> State
step m s pos
    | null s' = M.insert pos risk s
    | otherwise = M.insert pos (minimum s' + risk) s
    where
        risk = m M.! pos
        right = s M.!? first (+1) pos
        down = s M.!? second (+1) pos
        s' = catMaybes [right, down]


solve :: Input -> State
solve m = foldl' (step m) M.empty order
    where
        maxX = maximum $ fst <$> M.keys m
        maxY = maximum $ snd <$> M.keys m
        order = reverse $ cartesian2 [0..maxX] [0..maxY]


solve1 :: Input -> Int
solve1 m = risks M.! (0,0) - m M.! (0,0)
    where risks = solve m

clip :: Int -> Int
clip n = (n-1) `mod` 9 + 1

shiftMap :: (Int,Int) -> Input -> Input
shiftMap (dx,dy) m = M.mapKeys (bimap (dx*dim+) (dy*dim+)) $ M.map (clip . (dx+dy+)) m
    where dim = length $ nub $ fst <$> M.keys m

expand :: Input -> Input
expand m = M.unions $ flip shiftMap m <$> cartesian2 [0..4] [0..4]

solve2 = solve1 . expand

showGrid :: Input -> String
showGrid m = unlines $ fmap concat $ chunks dim $ M.elems $ M.mapKeys swap $ M.map show m
    where dim = length $ nub $ fst <$> M.keys m

main' :: String -> IO ()
main' raw = do
    let input = parse raw
    -- print $ expand $ input
    print $ length $ expand input
    -- print $ showGrid $ expand input
    print $ solve1 input
    -- print $ uncurry (-) $ solve2 input
    print $ solve2 input


run :: IO ()
run = readFile "in/15.txt" >>= main'

test :: IO ()
test = readFile "in/15t.txt" >>= main'

main :: IO ()
main = getContents >>= main'

