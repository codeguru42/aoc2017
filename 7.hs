parseLine l = (name, weight, children)
    where (name:weight':rest) = words l
          weight = init $ tail weight'
          children | rest == [] = []
                   | otherwise = [init child' | child' <- children']
                        where (_:children') = rest

parseAll = map parseLine . lines

children  = foldr (\(_, _, cs) xs -> cs ++ xs) []

main = do
    input <- readFile "7.txt"
    let nodes = parseAll input
    print $ children nodes
