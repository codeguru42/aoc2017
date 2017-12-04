import Test.HUnit
import Data.Text as T

tests = test [testValidPassPhrase, testInvalidPassPhrase]
testValidPassPhrase = True ~=? isValidPassPhrase "aa bb cc dd ee"
testInvalidPassPhrase = False ~=? isValidPassPhrase "aa bb cc dd aa"

isValidPassPhrase s = noDupes xs
    where xs = T.splitOn (pack " ") (pack s)
          noDupes [] = True
          noDupes (y:ys) = (not $ y `elem` ys) && (noDupes ys)

main = do
    runTestTT tests
    input <- readFile "4.txt"
    print . Prelude.length . Prelude.filter isValidPassPhrase $ Prelude.lines input
