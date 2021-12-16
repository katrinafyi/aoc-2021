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

hexToBits :: Char -> String
hexToBits '0' = "0000"
hexToBits '1' = "0001"
hexToBits '2' = "0010"
hexToBits '3' = "0011"
hexToBits '4' = "0100"
hexToBits '5' = "0101"
hexToBits '6' = "0110"
hexToBits '7' = "0111"
hexToBits '8' = "1000"
hexToBits '9' = "1001"
hexToBits 'A' = "1010"
hexToBits 'B' = "1011"
hexToBits 'C' = "1100"
hexToBits 'D' = "1101"
hexToBits 'E' = "1110"
hexToBits 'F' = "1111"
hexToBits _ = ""

charToInt :: Char -> Int
charToInt = read . pure

bitsToNum :: [Int] -> Int
bitsToNum = foldl' ((+) . (*2)) 0

type Input = String

parse :: [Char] -> Input
parse = concatMap hexToBits


data Packet =
    Literal { packVersion :: Int, packType :: Int, packValue :: Int }
    | Operator { packVersion :: Int, packType :: Int, packLenType :: Int, packLenNum :: Int, packPacks :: [Packet] }
    deriving (Eq, Show)

parseBit :: ReadP Int
parseBit = charToInt <$> char '0' +++ char '1'

parseBitsLen :: Int -> ReadP [Int]
parseBitsLen n = P.count n parseBit

parseNumLen :: Int -> ReadP Int
parseNumLen = fmap bitsToNum . parseBitsLen

parseGroup :: Int -> ReadP [Int]
parseGroup first = do
    n <- parseBit
    guard $ first == n
    parseBitsLen 4

parsePacksNum :: Int -> ReadP [Packet]
parsePacksNum 0 = pure []
parsePacksNum n = (:) <$> parsePacket <*> parsePacksNum (n-1)

parsePacksLen :: Int -> ReadP [Packet]
parsePacksLen l
    | l < 0 = error "negative length reached"
parsePacksLen 0 = pure []
parsePacksLen l = do
    (s, pack) <- gather parsePacket
    (pack:) <$> parsePacksLen (l - length s)

parseSubpackets :: ReadP Packet
parseSubpackets = do
    lenTyp <- parseBit
    let len = if lenTyp == 0 then 15 else 11
    n <- parseNumLen len
    Operator 0 0 lenTyp n <$> if lenTyp == 0
        then parsePacksLen n
        else parsePacksNum n

parseLiteral :: ReadP Packet
parseLiteral = do
    ver <- parseNumLen 3
    typ <- parseNumLen 3
    guard $ typ == 4

    front <- concat <$> many (parseGroup 1)
    back <- parseGroup 0

    let val = bitsToNum $ front ++ back
    pure $ Literal ver typ val

parseOperator :: ReadP Packet
parseOperator = do
    ver <- parseNumLen 3
    typ <- parseNumLen 3
    guard $ typ /= 4

    pack <- parseSubpackets
    pure $ pack { packVersion = ver, packType = typ }

nextMultiple :: Int -> Int -> Int
nextMultiple b x = x + (b - x `mod` b) `mod` b

parsePacket :: ReadP Packet
parsePacket = parseLiteral +++ parseOperator

parseMain :: ReadP Packet
parseMain = parsePacket <* many (char '0') <* eof

foldPacket :: (Packet -> a -> a) -> a -> Packet -> a
foldPacket f x p@(Operator _ _ _ _ ps) = f p (foldr (flip $ foldPacket f) x ps)
foldPacket f x p = f p x

packetToList :: Packet -> [Packet]
packetToList = foldPacket (:) []

readPacket :: String -> Packet
readPacket = fst . one . readP_to_S parseMain

solve1 :: String -> Int
solve1 = sum . fmap packVersion . packetToList . readPacket

evalPacket :: Packet -> Int
evalPacket p@(Literal _ _ v) = v
evalPacket p@(Operator _ t _ _ ps) = case t of
    0 -> sum ns
    1 -> product ns
    2 -> minimum ns
    3 -> maximum ns
    5 -> binary (>)
    6 -> binary (<)
    7 -> binary (==)
    _ -> undefined
    where
        ns = evalPacket <$> ps
        binary f = bool 0 1 $ uncurry f $ two ns

solve2 :: String -> Int
solve2 = evalPacket . readPacket

main' :: String -> IO ()
main' raw = do
    let input = parse raw
    print input
    print $ readPacket input

    print $ solve1 input
    print $ solve2 input

run :: IO ()
run = readFile "in/16.txt" >>= main'

test :: IO ()
test = readFile "in/16t.txt" >>= main'

main :: IO ()
main = getContents >>= main'

