import Test.HUnit

allTests = test [testSteps, testCoords, testSumNeighbors,testNeighbors]

stepsTestCases = [(1, 0), (12, 3), (23, 2), (1024, 31)]
makeTest f (input, expected) = expected ~=? f input
mapTests f = test . map (makeTest f)
testSteps = mapTests steps stepsTestCases

coordsTestCases = [(1, (0, 0)), (2, (1, 0)), (5, (-1, 1)), (12, (2, 1))]
testCoords = mapTests coords coordsTestCases

neighborsTestCases = [(1, []), (2, [1]), (3, [1, 2]), (4, [1, 2, 3]), (5, [1, 4]), (6, [1, 4, 5]), (7, [1, 6]), (8, [1, 6, 7])]
testNeighbors = mapTests neighbors neighborsTestCases

sumNeighborsTestCases = [(1, 1), (2, 1), (3, 2), (4, 4), (5, 5), (6, 10), (7, 11)]
testSumNeighbors = mapTests sumNeighbors sumNeighborsTestCases

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

neighbors :: Int -> [Int]
neighbors = undefined

sumNeighbors :: Int -> Int
sumNeighbors 1 = 1
sumNeighbors n = sum . map sumNeighbors $ neighbors n

lbSum n = head . filter (>n) $ map sumNeighbors [1..]

main = do
    runTestTT allTests
    print $ steps 368078
    print $ lbSum 368078