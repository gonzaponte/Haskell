rpn :: (Fractional a, Read a) => [a] -> String -> [a]
rpn xs y
    | y == "+"  = ((+) arg1 arg2) : stack
    | y == "-"  = ((-) arg1 arg2) : stack
    | y == "*"  = ((*) arg1 arg2) : stack
    | y == "/"  = ((/) arg1 arg2) : stack
    | otherwise =        (read y) : xs
    where arg2:arg1:stack = xs


solveRPN :: (Fractional a, Read a) => String -> a
solveRPN = head . (foldl rpn []) . words


group3 :: [Int] -> [(Int, Int, Int)]
group3 [] = []
group3 x
    | (length x) `mod` 3 == 0 = let (a:b:c:rest) = x in (a, b, c) : group3 rest
    | (length x) `mod` 3 == 1 = group3 $ x ++ [0, 0]
    | (length x) `mod` 3 == 2 = group3 $ x ++ [0]


crossRoad :: [(Int, Int, Int)] -> [(Int, Int, Int)]
crossRoad []             = []
crossRoad ((a, b, c):xs) = (b, a, c):crossRoad xs


shortestPath :: [(Int, Int, Int)] -> Int
shortestPath [] = 0
shortestPath (step:rest) = let (fwda, fwdb, cross) = step
                           in min (fwda + shortestPath rest) (cross + fwdb + shortestPath (crossRoad rest))
