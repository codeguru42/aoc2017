import Data.Char (digitToInt)
import Data.Maybe (catMaybes)

pairs1 :: [Int] -> [(Int, Int)]
pairs1 xs = zip xs (tail xs ++ [head xs])

pairs2 :: [Int] -> [(Int, Int)]
pairs2 xs = zip xs (drop n xs ++ take n xs)
    where n = length xs `div` 2

matches :: [(Int, Int)] -> [Maybe Int]
matches = map (\(x, y) -> if x == y then Just x else Nothing)

main = do
    input <- readFile "1.txt"
    let digits = map digitToInt input
    let doIt = sum . catMaybes . matches
    print . doIt $ pairs1 digits
    print . doIt $ pairs2 digits
