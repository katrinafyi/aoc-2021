module Main where

import Numeric
import Data.List
import Data.Char
import Data.Maybe
import Text.Read(readMaybe)
import Control.Applicative
import qualified Data.Map.Strict as M

import Lib

type Coord = (Int, Int)

type State = M.Map Coord Int

data Segment = Segment Coord Coord deriving (Eq, Show)

parseCoord :: String -> Coord
parseCoord s = (read x,read y)
    where
        [x, y] = words $ replace ',' ' ' s

parseLine :: String -> Maybe Segment
parseLine s = case words s of
    [from, _, to] -> Just $ Segment (parseCoord from) (parseCoord to)
    _ -> Nothing

parse :: String -> [Segment]
parse raw = catMaybes $ parseLine <$> lines raw

interpolate :: Segment -> [Coord]
interpolate (Segment (x1,y1) (x2,y2))
    | (x1,y1) == (x2,y2) = [(x1,y1)]
    | otherwise = (x1,y1) : interpolate (Segment (x1+dx,y1+dy) (x2,y2))
    where
        len = abs (x1-x2) `max` abs (y1-y2)
        dx = (x2-x1) `div` len
        dy = (y2-y1) `div` len

addSegment :: M.Map Coord Int -> Segment -> M.Map Coord Int
addSegment m seg = foldl (flip increment) m (interpolate seg)

isHorVert :: Segment -> Bool
isHorVert (Segment (x1,y1) (x2,y2))
    | x1 == x2 = True
    | y1 == y2 = True
    | otherwise = False

solve1 :: [Segment] -> M.Map Coord Int
solve1 segs = foldl addSegment M.empty s'
    where s' = filter isHorVert segs

solve2 :: [Segment] -> M.Map Coord Int
solve2 segs = foldl addSegment M.empty segs

main :: IO ()
main = do
    input <- parse <$> getContents
    print $ input

    -- print $ solve1 input
    print $ length $ M.filter (>= 2) $ solve1 input
    print $ length $ M.filter (>= 2) $ solve2 input
    -- print $ solve1 input
    -- print $ solve2 input

