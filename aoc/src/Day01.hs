import Lib

solve :: Int -> [Int] -> Int
solve w xs = length $ filter id $ zipWith (<) sums (tail sums)
    where sums = sum <$> sliding w xs

solve1 = solve 1
solve2 = solve 3

parse = fmap read . lines

main :: IO ()
main = do
    input <- parse <$> getContents
    print $ solve1 input
    print $ solve2 input

