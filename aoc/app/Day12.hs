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

parse :: [Char] -> Graph
parse raw = foldl (\m (f,t) -> M.insertWith (++) f [t] m) M.empty ls'
    where
        ls = fmap words $ lines $ replace '-' ' ' raw
        ls' = two <$> ls ++ fmap reverse ls

isBig :: [Char] -> Bool
isBig = all isUpper . take 1

isSmall :: [Char] -> Bool
isSmall = not . isBig

go :: Graph -> S.Set String -> Maybe String -> String -> [[String]]
go _ _ _ "end" = [["end"]]
go m s t x = do
    guard $ t /= Just "start"
    guard $ isBig x || x `S.notMember` s || isNothing t
    let t' = if isSmall x && x `S.member` s then Just x else t
    x' <- fromMaybe [] $ m M.!? x
    (x:) <$> go m (S.insert x s) t' x'

allPaths :: Graph -> [[String]]
allPaths m = go m S.empty (Just "") "start"

allPaths2 :: Graph -> [[String]]
allPaths2 m = go m S.empty Nothing "start"

solve1 :: Graph -> Int
solve1 = length . allPaths

solve2 :: Graph -> Int
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

