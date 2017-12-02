import Test.HUnit
import Data.Text as T

allTests = test [testSplitOnTabs
                , testSplitAll
                , testParse
                , testPart1
                , testChecksum1
                , testAllPairs
                , testIsDivisibleFalse
                , testIsDivisibleTrue
                , testDivisiblePairs
                , testPart2
                , testChecksum2
                ]
testSplitOnTabs = "testSplitOnTabs" ~: [pack "foo", pack "bar"] ~=? (splitOnTabs $ pack "foo\tbar")
testSplitAll = "testSplitAll" ~: [ [pack "foo", pack "bar"]
                                   , [pack "fooze", pack "baz"]
                                 ]
                              ~=? splitAll "foo\tbar\nfooze\tbaz\n"
testParse = "testParse" ~: [[1, 2], [3, 4]] ~=? parse "1\t2\n3\t4\n"
testPart1 = "testPart1" ~: 6 ~=? part1 [2, 7, 8, 3, 6, 4]
testChecksum1 = "testChecksum1" ~: 8 ~=? checksum1 "2\t7\t 5\n3\t6\t4\n"
testAllPairs = "testAllPairs" ~: [(1, 2), (1, 3), (2, 3)] ~=? allPairs [1, 2, 3]
testIsDivisibleFalse = "testIsDivisible" ~: False ~=? isDivisible (3, 5)
testIsDivisibleTrue = "testIsDivisible" ~: True ~=? isDivisible (3, 12)
testDivisiblePairs = "testDivisiblePairs" ~: [(2, 8), (2, 6), (2, 4), (8, 4), (3, 6)] ~=? divisiblePairs [2, 7, 8, 3, 6, 4]
testPart2 = "testPart2" ~: 4 ~=? part2 [5, 12, 7, 3]
testChecksum2 = "testChecksum2" ~: 9 ~=? checksum2 "5\t9\t2\t8\n9\t4\t7\t3\n3\t8\t6\t5\n"

splitOnTabs = T.splitOn (pack "\t")
splitAll = (Prelude.map splitOnTabs) . T.lines . pack
parse = Prelude.map (Prelude.map $ (read :: String -> Int) . unpack) . splitAll

part1 xs = Prelude.maximum xs - Prelude.minimum xs
checksum f = sum . (Prelude.map f) . parse
checksum1 = checksum part1

allPairs [] = []
allPairs (x:xs) = Prelude.map (\y -> (x,y)) xs ++ allPairs xs

isDivisible (x,y) = x `mod` y == 0 || y `mod` x == 0
divisiblePairs = Prelude.filter isDivisible . allPairs
part2 xs = (max x y) `div` (min x y)
    where (x, y) = Prelude.head $ divisiblePairs xs
checksum2 = checksum part2

main = do
    runTestTT allTests
    input <- readFile "2.txt"
    print $ checksum1 input
    print $ checksum2 input
