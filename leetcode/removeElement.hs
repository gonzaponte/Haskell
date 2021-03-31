
removeElement' :: (Eq a) => a -> [a] -> [a]
removeElement' _ [] = []
removeElement' v (x:xs)
    | x == v    = rest
    | otherwise = x:rest
    where rest  = removeElement' v xs

removeElement :: (Eq a) => a -> [a] -> Int
removeElement v = length . removeElement' v
