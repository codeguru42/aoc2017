import Test.HUnit

tests = test [testNewInstructions]
newInstructionsFixtures = [ ([1, 3, 0, 1, -3], 0, [0, 3, 0, 1, -3])
                          , ([2, 3, 0, 1, -3], 0, [1, 3, 0, 1, -3])
                          , ([2, 4, 0, 1, -3], 1, [2, 3, 0, 1, -3])
                          , ([2, 4, 0, 1, -2], 4, [2, 4, 0, 1, -3])
                          , ([2, 5, 0, 1, -2], 1, [2, 4, 0, 1, -2])
                          ]
makeNewInstructionsTest (expected, i, instructions) = expected ~=? newInstructions i instructions
testNewInstructions = test $ map makeNewInstructionsTest newInstructionsFixtures

newInstructions i instructions = take i instructions ++ [j + 1] ++ drop (i + 1) instructions
    where j = instructions !! i

main = do
    runTestTT tests
