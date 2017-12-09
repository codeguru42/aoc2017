parseLine l = (name, weight, children)
    where (name:weight':rest) = words l
          weight = init $ tail weight'
          children | rest == [] = []
                   | otherwise = [init child' | child' <- children']
                        where (_:children') = rest

parseAll = map parseLine . lines

main = do
    input <- readFile "7.txt"
    print $ parseAll input
