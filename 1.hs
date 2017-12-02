import Data.Char (digitToInt)
import Data.Maybe (catMaybes)
import Test.HUnit

part1Fixtures = [ ("test1_1", 3, [1, 1, 2, 2])
                , ("test1_2", 4, [1, 1, 1, 1])
                , ("test1_3", 0, [1, 2, 3, 4])
                , ("test1_1", 9, [9, 1, 2, 1, 2, 9])
                ]
part1Test (label, expected, input) = label ~: expected ~=? (part1 input)
part1Tests = test $ map part1Test part1Fixtures

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
    runTestTT part1Tests
    input <- readFile "1.txt"
    let digits = map digitToInt input
    print $ part1 digits
    print $ part2 digits
