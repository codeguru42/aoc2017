import Data.Char (digitToInt)
import Data.Maybe (catMaybes)
import Test.HUnit

tests = test [test1_1, test1_2, test1_3, test1_4]
test1_1 = "test1_1" ~: 3 ~=? (part1 [1, 1, 2, 2])
test1_2 = "test1_2" ~: 4 ~=? (part1 [1, 1, 1, 1])
test1_3 = "test1_3" ~: 0 ~=? (part1 [1, 2, 3, 4])
test1_4 = "test1_1" ~: 9 ~=? (part1 [9, 1, 2, 1, 2, 9])

pairs1 :: [Int] -> [(Int, Int)]
pairs1 xs = zip xs (tail xs ++ [head xs])

pairs2 :: [Int] -> [(Int, Int)]
pairs2 xs = zip xs (drop n xs ++ take n xs)
    where n = length xs `div` 2

matches :: [(Int, Int)] -> [Maybe Int]
matches = map (\(x, y) -> if x == y then Just x else Nothing)

day1 ::  [(Int, Int)] -> Int
day1 = sum . catMaybes . matches

part1 :: [Int] -> Int
part1 = day1 . pairs1

part2 :: [Int] -> Int
part2 = day1 . pairs2

main = do
    runTestTT tests
    input <- readFile "1.txt"
    let digits = map digitToInt input
    print $ part1 digits
    print $ part2 digits
