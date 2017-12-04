import Test.HUnit
import Data.List (sort)
import Data.Text as T

tests = test [testValidPassPhrase1, testInvalidPassPhrase1, testIsAnagram, testIsNotAnagram]
testValidPassPhrase1 = True ~=? isValidPassPhrase1 "aa bb cc dd ee"
testInvalidPassPhrase1 = False ~=? isValidPassPhrase1 "aa bb cc dd aa"
testIsAnagram = True ~=? isAnagramOf "abcde" "ecdab"
testIsNotAnagram = False ~=? isAnagramOf "abcde" "fghij"

validate1 y ys = (not $ y `elem` ys)
isValidPassPhrase1 = isValidPassPhrase validate1

isValidPassPhrase f s = validate xs
    where xs = T.splitOn (pack " ") (pack s)
          validate [] = True
          validate (y:ys) = (f y ys) && (validate ys)
countValidPassPhrases f = Prelude.length . Prelude.filter f

isAnagramOf x y = (sort x) == (sort y)

main = do
    runTestTT tests
    input <- readFile "4.txt"
    print . countValidPassPhrases isValidPassPhrase1 $ Prelude.lines input
