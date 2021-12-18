module Main where

import Numeric
import Data.List
import Data.Char
import Data.Maybe
import Text.Read(readMaybe)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Sequence as SS

import Lib
import Data.Function
import Control.Monad (guard)
import Data.Bifunctor
import Data.Tuple
import Data.Bool
import Text.ParserCombinators.ReadP as P


data Pair a = One a | Two (Pair a) (Pair a)
    deriving (Eq, Show)

data Step f = L f | R f deriving (Eq, Show)



type PairCtx a = (Pair a, [Step (Pair a)])

parseOne :: ReadP (Pair Int)
parseOne = One . read <$> munch1 isDigit

parseTwo :: ReadP (Pair Int)
parseTwo = between (char '[') (char ']') $
    Two <$> parsePair <* char ',' <*> parsePair

parsePair :: ReadP (Pair Int)
parsePair = parseOne +++ parseTwo


parse :: String -> [Pair Int]
parse = fmap (runParser parsePair) . lines

runParser :: ReadP a -> String -> a
runParser p = fst . one . readP_to_S p


main' :: String -> IO ()
main' raw = do
    let input = parse raw
    print input

run :: IO ()
run = readFile "in/18.txt" >>= main'

test :: IO ()
test = readFile "in/18t.txt" >>= main'

main :: IO ()
main = getContents >>= main'

