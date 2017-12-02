import Test.HUnit
import Data.Text as T

allTests = test [testSplitOnTabs, testSplitAll, testParse, testDiff]
testSplitOnTabs = "testSplitOnTabs" ~: [pack "foo", pack "bar"] ~=? (splitOnTabs $ pack "foo\tbar")
testSplitAll = "testSplitAll" ~: [ [pack "foo", pack "bar"]
                                   , [pack "fooze", pack "baz"]
                                 ]
                              ~=? splitAll "foo\tbar\nfooze\tbaz\n"
testParse = "testParse" ~: [[1, 2], [3, 4]] ~=? parse "1\t2\n3\t4\n"
testDiff = "testDiff" ~: 6 ~=? diff [2, 7, 8, 3, 6, 4]

splitOnTabs = T.splitOn (pack "\t")
splitAll = (Prelude.map splitOnTabs) . T.lines . pack
parse = Prelude.map (Prelude.map $ (read :: String -> Int) . unpack) . splitAll

diff xs = Prelude.maximum xs - Prelude.minimum xs

main = do
    runTestTT allTests
    input <- readFile "2.txt"
    print $ parse input
