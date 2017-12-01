import Data.Char (digitToInt)
import Data.Maybe (catMaybes)

pairs xs = zip xs (tail xs ++ [head xs])

matches :: [(Int, Int)] -> [Maybe Int]
matches = map (\(x, y) -> if x == y then Just x else Nothing)

main = do
    input <- readFile "1.1.txt"
    let digits = map digitToInt input
    print . sum . catMaybes . matches $ pairs digits
