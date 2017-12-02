import Data.Char (digitToInt)
import Data.Maybe (catMaybes)
import Test.HUnit

part1Fixtures = [ ("test1_1", 3, "1122")
                , ("test1_2", 4, "1111")
                , ("test1_3", 0, "1234")
                , ("test1_1", 9, "912129")
                ]
part1Test (label, expected, input) = label ~: expected ~=? (part1 input)
part1Tests = test $ map part1Test part1Fixtures

pairs1 xs = zip xs (tail xs ++ [head xs])
pairs2 xs = zip xs (drop n xs ++ take n xs)
    where n = length xs `div` 2
matches = map (\(x, y) -> if x == y then Just x else Nothing)

day1 f = sum . catMaybes . matches . f . parse
part1 = day1 pairs1
part2 = day1 pairs2
parse = map digitToInt

main = do
    runTestTT part1Tests
    input <- readFile "1.txt"
    print $ part1 input
    print $ part2 input
