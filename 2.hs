import Test.HUnit
import Data.Text as T

allTests = test [testSplitOnTabs]
testSplitOnTabs = "testSplitOnTabs" ~: [pack "foo", pack "bar"] ~=? (splitOnTabs $ pack "foo\tbar")

splitOnTabs = T.splitOn (pack "\t")

main = do
    runTestTT allTests
