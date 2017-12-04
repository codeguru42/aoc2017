import Test.HUnit
import Data.List (sort)
import Data.Text as T

tests = test [testValidPassPhrase, testInvalidPassPhrase, testIsAnagram, testIsNotAnagram]
testValidPassPhrase = True ~=? isValidPassPhrase "aa bb cc dd ee"
testInvalidPassPhrase = False ~=? isValidPassPhrase "aa bb cc dd aa"
testIsAnagram = True ~=? isAnagramOf "abcde" "ecdab"
testIsNotAnagram = False ~=? isAnagramOf "abcde" "fghij"

isValidPassPhrase s = noDupes xs
    where xs = T.splitOn (pack " ") (pack s)
          noDupes [] = True
          noDupes (y:ys) = (not $ y `elem` ys) && (noDupes ys)

isAnagramOf x y = (sort x) == (sort y)

main = do
    runTestTT tests
    input <- readFile "4.txt"
    print . Prelude.length . Prelude.filter isValidPassPhrase $ Prelude.lines input
