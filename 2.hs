import Test.HUnit
import Data.Text as T

allTests = test [testSplitOnTabs, testSplitAll, testParse, testDiff, testDiffAll, testChecksum]
testSplitOnTabs = "testSplitOnTabs" ~: [pack "foo", pack "bar"] ~=? (splitOnTabs $ pack "foo\tbar")
testSplitAll = "testSplitAll" ~: [ [pack "foo", pack "bar"]
                                   , [pack "fooze", pack "baz"]
                                 ]
                              ~=? splitAll "foo\tbar\nfooze\tbaz\n"
testParse = "testParse" ~: [[1, 2], [3, 4]] ~=? parse "1\t2\n3\t4\n"
testDiff = "testDiff" ~: 6 ~=? diff [2, 7, 8, 3, 6, 4]
testDiffAll = "testDiffAll" ~: [5, 3] ~=? diffAll [[2, 7, 5], [3, 6, 4]]
testChecksum = "testChecksum" ~: 8 ~=? checksum "2\t7\t 5\n3\t6\t4\n"

splitOnTabs = T.splitOn (pack "\t")
splitAll = (Prelude.map splitOnTabs) . T.lines . pack
parse = Prelude.map (Prelude.map $ (read :: String -> Int) . unpack) . splitAll

diff xs = Prelude.maximum xs - Prelude.minimum xs
diffAll = Prelude.map diff
checksum = sum . diffAll . parse

main = do
    runTestTT allTests
    input <- readFile "2.txt"
    print $ parse input
