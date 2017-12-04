import Test.HUnit
import Data.List (sort)

tests = test [testValidPassPhrase1, testInvalidPassPhrase1, testIsAnagram, testIsNotAnagram]
testValidPassPhrase1 = True ~=? isValidPassPhrase1 "aa bb cc dd ee"
testInvalidPassPhrase1 = False ~=? isValidPassPhrase1 "aa bb cc dd aa"
testIsAnagram = True ~=? isAnagramOf "abcde" "ecdab"
testIsNotAnagram = False ~=? isAnagramOf "abcde" "fghij"

validate1 [] = True
validate1 (y:ys) = (not $ y `elem` ys) && (validate1 ys)
isValidPassPhrase1 = isValidPassPhrase validate1

isValidPassPhrase validate s = validate $ words s
countValidPassPhrases f = length . filter f

isAnagramOf x y = (sort x) == (sort y)

main = do
    runTestTT tests
    input <- readFile "4.txt"
    print . countValidPassPhrases isValidPassPhrase1 $ lines input
