import Test.HUnit
import Data.List (sort)

tests = test [testValidPassPhrase1, testInvalidPassPhrase1]
testValidPassPhrase1 = True ~=? isValidPassPhrase1 "aa bb cc dd ee"
testInvalidPassPhrase1 = False ~=? isValidPassPhrase1 "aa bb cc dd aa"

validate1 [] = True
validate1 (y:ys) = (not $ y `elem` ys) && (validate1 ys)
isValidPassPhrase1 = isValidPassPhrase validate1

isValidPassPhrase validate s = validate $ words s
countValidPassPhrases f = length . filter f

validate2 xs = validate1 xs'
    where xs' = map sort xs
isValidPassPhrase2 = isValidPassPhrase validate2

main = do
    runTestTT tests
    input <- readFile "4.txt"
    print . countValidPassPhrases isValidPassPhrase1 $ lines input
    print . countValidPassPhrases isValidPassPhrase2 $ lines input
