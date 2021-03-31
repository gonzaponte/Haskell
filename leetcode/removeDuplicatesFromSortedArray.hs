
nub' :: (Eq a) => [a] -> [a]
nub' [] = []
nub' (x:xs) = let rest = dropWhile (x==) xs
              in x : nub' rest

removeDuplicatesFromSortedArray :: (Eq a) => [a] -> Int
removeDuplicatesFromSortedArray = length . nub'
