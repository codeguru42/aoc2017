import Test.HUnit
import Data.Text as T

allTests = test [testSplitOnTabs, testParse]
testSplitOnTabs = "testSplitOnTabs" ~: [pack "foo", pack "bar"] ~=? (splitOnTabs $ pack "foo\tbar")
testParse = "testParse" ~: [ [pack "foo", pack "bar"]
                           , [pack "fooze", pack "baz"]
                           ]
                        ~=? parse "foo\tbar\nfooze\tbaz\n"

splitOnTabs = T.splitOn (pack "\t")

parse = (Prelude.map splitOnTabs) . T.lines . pack

main = do
    runTestTT allTests
    -- input <- readFile "1.txt"
    --print $ parse input
