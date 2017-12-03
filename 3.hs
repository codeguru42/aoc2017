import Test.HUnit

testCases :: [(Int, Int)]
testCases = [(1, 0), (12, 3), (23, 2), (1024, 31)]
makeTest (input, expected) = expected ~=? steps input
tests = test $ map makeTest testCases

coords :: Int -> (Int, Int)
coords = undefined

steps :: Int -> Int
steps n = abs x + abs y
    where (x, y) = coords n

main = do
    runTestTT tests
    print $ steps 368078
