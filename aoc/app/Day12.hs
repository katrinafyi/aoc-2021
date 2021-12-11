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

parse :: [Char] -> M.Map String [String]
parse raw = foldl (\m ft -> M.insertWith (++) (ft!!0) [ft!!1] m) M.empty ls'
    where
        ls = fmap words $ lines $ replace '-' ' ' raw
        ls' = ls ++ fmap reverse ls

isBig :: [Char] -> Bool
isBig = all isUpper . take 1

isSmall :: [Char] -> Bool
isSmall = not . isBig

cartesian2 :: [a] -> [b] -> [(a,b)]
cartesian2 = liftA2 (,)

allPaths :: M.Map String [String] -> [[String]]
allPaths m = go S.empty "start"
    where
        go :: S.Set String -> String -> [[String]]
        go _ "end" = [["end"]]
        go s x = do
            guard $ isBig x || x `S.notMember` s
            x' <- fromMaybe [] $ m M.!? x
            (x:) <$> go (S.insert x s) x'

allPaths2 :: M.Map String [String] -> [[String]]
allPaths2 m = go S.empty Nothing "start"
    where
        go :: S.Set String -> Maybe String -> String -> [[String]]
        go _ _ "end" = [["end"]]
        go s t x = do
            guard $ t /= Just "start"
            guard $ isBig x || x `S.notMember` s || isNothing t
            let t' = if isSmall x && x `S.member` s then Just x else t
            x' <- fromMaybe [] $ m M.!? x
            (x:) <$> go (S.insert x s) t' x'

solve1 :: M.Map String [String] -> Int
solve1 = length . allPaths

solve2 :: M.Map String [String] -> Int
solve2 = length . allPaths2

main' :: String -> IO ()
main' raw = do
    let input = parse raw
    print $ input
    -- mapM_ putStrLn $ intercalate "," <$> allPaths2 input

    print $ solve1 input
    print $ solve2 input

run :: IO ()
run = readFile "in/12.txt" >>= main'

test :: IO ()
test = readFile "in/12t.txt" >>= main'

main :: IO ()
main = getContents >>= main'

