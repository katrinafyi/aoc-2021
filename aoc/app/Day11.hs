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

type Board = M.Map (Int,Int) Int

parse :: String -> Board
parse = M.fromList . makeGrid . fmap (fmap (read . pure)) . lines

adjacents8 :: (Int,Int) -> [(Int,Int)]
adjacents8 (x,y) = do
        [dx,dy] <- deltas
        guard $ (dx,dy) /= (0,0)
        pure (x+dx,y+dy)
    where
        deltas = sequence [[-1, 0, 1], [-1, 0, 1]]

flash :: Board -> (Board, Board) -- returns (board', flashed)
flash = go M.empty . fmap (+1)
    where
        go :: Board -> Board -> (Board, Board)
        go f m
            | null nines = (m, 0 <$ f)
            | otherwise = go (M.union f nines) m'
            where
                (nines, nonNine) = M.partition (>9) m
                adjs = concatMap adjacents8 $ M.keys nines
                m' = foldr (M.adjust (+1)) nonNine adjs


step :: Board -> (Int, Board)
step m = (length f, M.union m' f)
    where (m', f) = flash m

step' :: (Int, Board) -> (Int, Board)
step' (n, b) = first (+n) $ step b

solve1 :: Board -> (Int, Board)
solve1 b = iterate step' (0, b) !! 100

solve2 :: Board -> (Int, Board)
solve2 m = bimap length head $ break (all (==0)) steps
    where steps = snd <$> iterate step' (0, m)

main' :: String -> IO ()
main' raw = do
    let input = parse raw
    print $ input
    -- print $ flash input

    -- print $ M.elems $ snd $ step 2 input
    print $ solve1 input
    print $ solve2 input

run :: IO ()
run = readFile "in/11.txt" >>= main'

test :: IO ()
test = readFile "in/11t.txt" >>= main'

main :: IO ()
main = getContents >>= main'

