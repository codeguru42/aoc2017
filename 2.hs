import Test.HUnit
import Data.Text as T

testSplitOnTabs = "testSplitOnTabs" ~: [pack "foo", pack "bar"] ~=? (splitOnTabs "foo\tbar")

splitOnTabs = T.splitOn (pack "\t") . pack

main = do
    runTestTT testSplitOnTabs
