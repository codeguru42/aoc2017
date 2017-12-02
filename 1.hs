import Data.Char (digitToInt)
import Data.Maybe (catMaybes)
import Test.HUnit

allTests = test [part1Tests, part2Tests]

part1Fixtures = [ ("test1_1", 3, "1122")
                , ("test1_2", 4, "1111")
                , ("test1_3", 0, "1234")
                , ("test1_1", 9, "912129")
                ]
part1Test (label, expected, input) = label ~: expected ~=? (part1 input)
part1Tests = test $ map part1Test part1Fixtures

part2Fixtures = [ ("test2_1", 6, "1212")
                , ("test2_2", 0, "1221")
                , ("test2_3", 4, "123425")
                , ("test2_4", 12, "123123")
                , ("test2_5", 4, "12131415")
                ]
part2Test (label, expected, input) = label ~: expected ~=? (part2 input)
part2Tests = test $ map part2Test part2Fixtures

pairs1 xs = zip xs (tail xs ++ [head xs])
pairs2 xs = zip xs (drop n xs ++ take n xs)
    where n = length xs `div` 2
matches = map (\(x, y) -> if x == y then Just x else Nothing)

day1 f = sum . catMaybes . matches . f . parse
part1 = day1 pairs1
part2 = day1 pairs2
parse = map digitToInt

main = do
    runTestTT allTests
    input <- readFile "1.txt"
    print $ part1 input
    print $ part2 input
