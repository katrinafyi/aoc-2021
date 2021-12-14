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
        (top, bot) = break null ls
        ls = lines raw
        inserts = (\b -> (two $ b!!0, head $ b!!2)) <$> words <$> tail bot


go :: Rules -> String -> String
go m (x:y:xs) = case m M.!? (x,y) of
    Just c -> x:c:go m (y:xs)
    _ -> x:go m (y:xs)
go _ x = x

type State = M.Map (Char,Char) Int


go' :: Rules -> State -> State
go' rules s = M.unionWith (+) s delta
    where
        delta = M.fromList $ concatMap makeDelta $ M.toList rules
        makeDelta ((a,b),c) = [((a,b), -n), ((a,c), n), ((b,c), n)]
            where n = M.findWithDefault 0 (a,b) s

solve1 :: Int -> ([Char], Rules) -> ((Int,Int), (String, String))
solve1 n (s, m) = ((maxCount,minCount), (filter (\c -> maxCount == count (==c) s') chars,
        filter (\c -> minCount == count (==c) s') chars))
    where
        s' = iterate (go m) s !! n
        chars = nub s'
        minCount = minimum $ (\c -> count (==c) s') <$> chars
        maxCount = maximum $ (\c -> count (==c) s') <$> chars


main' :: String -> IO ()
main' raw = do
    let input = parse raw
    let (s, m) = input
    print $ input

    let twoDiff = \a c -> c-a*2

    let xs = (\n -> solve1 n input) <$> [1..15]
    print $ length $ nub $ iterate (go m) s !! 15
    print $ "asdf"
    print $ xs
    let results = fst <$> xs
    let subs = uncurry (-) <$> results
    mapM_ print $ subs
    mapM_ print $ results
    print $  zipWith (both2 twoDiff) results (tail results)
    print $  zipWith (twoDiff) subs (tail subs)


run :: IO ()
run = readFile "in/14.txt" >>= main'

test :: IO ()
test = readFile "in/14t.txt" >>= main'

main :: IO ()
main = getContents >>= main'

