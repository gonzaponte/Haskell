
mergeTwoSortedLists :: (Ord a) => [a] -> [a] -> [a]
mergeTwoSortedLists [] ys = ys
mergeTwoSortedLists xs [] = xs
mergeTwoSortedLists xall@(x:xs) yall@(y:ys)
    | y < x     = y : mergeTwoSortedLists xall ys
    | otherwise = x : mergeTwoSortedLists xs   yall
