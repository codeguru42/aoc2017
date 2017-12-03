import Test.HUnit

allTests = test [testSteps, testCoords]

stepsTestCases = [(1, 0), (12, 3), (23, 2), (1024, 31)]
makeTest f (input, expected) = expected ~=? f input
testSteps = test $ map (makeTest steps) stepsTestCases

coordsTestCases = [(1, (0, 0)), (2, (1, 0)), (5, (-1, 1)), (12, (2, 1))]
testCoords = test $ map (makeTest coords) coordsTestCases

coords :: Int -> (Int, Int)
coords 1 = (0, 0)
coords 2 = (1, 0)
coords 3 = (1, 1)
coords 4 = (0, 1)
coords n
    | n == sq = let (x, y) = coords ((evrt - 2) * (evrt - 2))
                in (x - 1, y + 1)
    | otherwise = let (x, y) = coords sq
                  in case group of
                    0 -> (x - 1, y - index)
                    1 -> (x + index, y - evrt)
                    2 -> (x + evrt, y - (evrt - 1) + index)
                    3 -> (x + (evrt - 1) - index, y + 1)
    where rt = floor . sqrt $ fromIntegral n
          evrt = if rt `mod` 2 == 0 then rt else rt - 1
          sq = evrt * evrt
          (x, y) = coords sq
          diff = n - sq - 1
          group = diff `div` (evrt + 1)
          index = diff `mod` (evrt + 1)

steps :: Int -> Int
steps n = abs x + abs y
    where (x, y) = coords n

main = do
    runTestTT allTests
    print $ steps 368078
