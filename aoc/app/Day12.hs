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

type Graph = M.Map String [String]

parse :: [Char] -> M.Map String (S.Set String)
parse raw = foldl (\m ft -> M.insertWith S.union (ft!!0) (S.singleton $ ft!!1) m) M.empty ls'
    where
        ls = fmap words $ lines $ replace '-' ' ' raw
        ls' = ls ++ fmap reverse ls

isBig :: [Char] -> Bool
isBig = all isUpper . take 1

cartesian2 :: [a] -> [b] -> [(a,b)]
cartesian2 = liftA2 (,)

connect :: M.Map String (S.Set String) -> String -> M.Map String (S.Set String)
connect m k = M.unionsWith (<>) [withoutBig, big, big2]
    where
        withoutBig = M.withoutKeys (fmap (S.filter (not . isBig)) m) outgoing
        incoming = fmap fst $ filter (S.member k . snd) $ M.toList m
        outgoing = M.findWithDefault S.empty k m
        big = M.fromListWith S.union $ second S.singleton <$> cartesian2 incoming (S.toList outgoing)
        big2 = M.fromListWith S.union $ second S.singleton <$> cartesian2 (S.toList outgoing) incoming

normalise :: M.Map String (S.Set String) -> M.Map String (S.Set String)
normalise m = foldl connect m bigs
    where bigs = filter (all isUpper) $ M.keys m


allPaths :: M.Map String (S.Set String) -> [[String]]
allPaths m = go m S.empty "start"
    where
        go :: M.Map String (S.Set String) -> S.Set String -> String -> [[String]]
        go m s = undefined

main' :: String -> IO ()
main' raw = do
    let input = parse raw
    print $ input
    print $ normalise $ input

    -- print $ solve1 input
    -- print $ solve2 input

run :: IO ()
run = readFile "in/12.txt" >>= main'

test :: IO ()
test = readFile "in/12t.txt" >>= main'

main :: IO ()
main = getContents >>= main'

