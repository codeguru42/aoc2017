import Data.Char (digitToInt)
import Data.Maybe (catMaybes)

pairs xs = zip xs (drop n xs ++ take n xs)
    where n = length xs `div` 2

matches :: [(Int, Int)] -> [Maybe Int]
matches = map (\(x, y) -> if x == y then Just x else Nothing)

main = do
    input <- readFile "1.txt"
    let digits = map digitToInt input
    print . sum . catMaybes . matches $ pairs digits
