import Test.HUnit

allTests = test [testSteps, testCoords]

stepsTestCases = [(1, 0), (12, 3), (23, 2), (1024, 31)]
makeTest f (input, expected) = expected ~=? f input
testSteps = test $ map (makeTest steps) stepsTestCases

coordsTestCases = [(1, (0, 0)), (2, (1, 0)), (5, (-1, 1)), (12, (2, 1))]
testCoords = test $ map (makeTest coords) coordsTestCases

coords :: Int -> (Int, Int)
coords = undefined

steps :: Int -> Int
steps n = abs x + abs y
    where (x, y) = coords n

main = do
    runTestTT allTests
    print $ steps 368078
