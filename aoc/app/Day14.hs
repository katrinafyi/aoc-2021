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

type Rules = M.Map (Char,Char) Char

parse :: [Char] -> ([Char], Rules)
parse raw = (head top, M.fromList inserts)
    where
        (top, bot) = break null $ lines raw
        inserts = (\b -> (two $ b!!0, head $ b!!2)) <$> words <$> tail bot


go :: Rules -> String -> String
go m (x:y:xs) = case m M.!? (x,y) of
    Just c -> x:c:go m (y:xs)
    _ -> x:go m (y:xs)
go _ x = x


solve1 :: ([Char], Rules) -> (Int,Int)
solve1 (s, m) = (maxCount,minCount)
    where
        s' = iterate (go m) s !! 10
        chars = nub s'
        minCount = minimum $ (\c -> count (==c) s') <$> chars
        maxCount = maximum $ (\c -> count (==c) s') <$> chars


type State = M.Map (Char,Char) Integer

delta :: Rules -> State -> State
delta rules s = M.fromListWith (+) $ concatMap makeDelta $ M.toList rules
    where
        makeDelta ((a,b),c) = [((a,b), -n), ((a,c), n), ((c,b), n)]
            where n = M.findWithDefault 0 (a,b) s

go' :: Rules -> State -> State
go' rules s = M.filter (/=0) $ M.unionWith (+) s (delta rules s)

toPairCounts :: String -> State
toPairCounts s' = M.fromListWith (+) $ (flip (,) 1) <$> zip s' (tail s')

solve2 :: ([Char], Rules) -> (Integer,Integer)
solve2 (s, m) = (maxCount, minCount)
    where
        pairs = toPairCounts $ "_" ++ s ++ "_"
        pairs' = iterate (go' m) pairs !! 40
        chars = filter (/= '_') $ nub $ fst <$> M.keys pairs'

        hasChar c (a,b) = a == c
        countChar c = sum $ M.filterWithKey (const . hasChar c) pairs'
        [maxCount, minCount] = sequence [maximum, minimum] $ countChar <$> chars


main' :: String -> IO ()
main' raw = do
    let input = parse raw
    let (s, m) = input
    print $ input

    print $ solve1 input
    print $ uncurry (-) $ solve2 input
    print $ solve2 input


run :: IO ()
run = readFile "in/14.txt" >>= main'

test :: IO ()
test = readFile "in/14t.txt" >>= main'

main :: IO ()
main = getContents >>= main'

