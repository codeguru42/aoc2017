import Test.HUnit
import Data.Text as T

tests = test [testValidPassPhrase, testInvalidPassPhrase]
testValidPassPhrase = True ~=? isValidPassPhrase "aa bb cc dd ee"
testInvalidPassPhrase = False ~=? isValidPassPhrase "aa bb cc dd aa"

isValidPassPhrase s = not $ x `elem` xs
    where (x:xs) = T.splitOn (pack " ") (pack s)

main = do
    runTestTT tests
