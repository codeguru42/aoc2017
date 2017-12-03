import Test.HUnit

testCases :: [(Int, Int)]
testCases = [(1, 0), (12, 3), (23, 2), (1024, 31)]
makeTest (input, expected) = expected ~=? steps input
tests = test $ map makeTest testCases

steps :: Int -> Int
steps = undefined

main = do
    runTestTT tests
    print $ steps 368078
