import Data.Char (digitToInt)

matches :: [Int] -> [Int]
matches (x:xs) = matches' $ x : xs ++ [x]

matches' :: [Int] -> [Int]
matches' [] = []
matches' (x:y:xs)
    | x == y = x : matches' xs
    | otherwise = matches' (y:xs)

main = do
    input <- readFile "1.1.txt"
    let digits = map digitToInt input
    print $ digits
    print . sum $ matches digits
