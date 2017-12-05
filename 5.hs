import Test.HUnit

tests = test [testNewInstructions]
testNewInstructions = [1, 3, 0, 1, -3] ~=? newInstructions 0 [0, 3, 0, 1, -3]

newInstructions i instructions = take i instructions ++ [j + 1] ++ drop (i + 1) instructions
    where j = instructions !! i

main = do
    runTestTT tests
