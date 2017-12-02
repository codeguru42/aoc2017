import Test.HUnit
import Data.Text as T

allTests = test [testSplitOnTabs, testSplitAll]
testSplitOnTabs = "testSplitOnTabs" ~: [pack "foo", pack "bar"] ~=? (splitOnTabs $ pack "foo\tbar")
testSplitAll = "testSplitAll" ~: [ [pack "foo", pack "bar"]
                                   , [pack "fooze", pack "baz"]
                                 ]
                              ~=? splitAll "foo\tbar\nfooze\tbaz\n"

splitOnTabs = T.splitOn (pack "\t")

splitAll = (Prelude.map splitOnTabs) . T.lines . pack

main = do
    runTestTT allTests
    -- input <- readFile "1.txt"
    --print $ parse input
