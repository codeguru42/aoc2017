import Test.HUnit
import Data.Text as T

allTests = test [testSplitOnTabs, testSplitAll, testParse, testPart1, testChecksum]
testSplitOnTabs = "testSplitOnTabs" ~: [pack "foo", pack "bar"] ~=? (splitOnTabs $ pack "foo\tbar")
testSplitAll = "testSplitAll" ~: [ [pack "foo", pack "bar"]
                                   , [pack "fooze", pack "baz"]
                                 ]
                              ~=? splitAll "foo\tbar\nfooze\tbaz\n"
testParse = "testParse" ~: [[1, 2], [3, 4]] ~=? parse "1\t2\n3\t4\n"
testPart1 = "testPart1" ~: 6 ~=? part1 [2, 7, 8, 3, 6, 4]
testChecksum = "testChecksum" ~: 8 ~=? checksum "2\t7\t 5\n3\t6\t4\n"

splitOnTabs = T.splitOn (pack "\t")
splitAll = (Prelude.map splitOnTabs) . T.lines . pack
parse = Prelude.map (Prelude.map $ (read :: String -> Int) . unpack) . splitAll

part1 xs = Prelude.maximum xs - Prelude.minimum xs
checksum = sum . (Prelude.map part1) . parse

main = do
    runTestTT allTests
    input <- readFile "2.txt"
    print $ checksum input
