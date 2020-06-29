duplicate_ :: [a] -> [a]
duplicate_ []     = []
duplicate_ (x:xs) = x : x : duplicate_ xs


replicate_ :: Int -> [a] -> [a]
replicate_ 0 _      = []
replicate_ _ []     = []
replicate_ 1 xs     = xs
replicate_ n (x:[]) = [x | _ <- [1..n]]
replicate_ n (x:xs) = (replicate_ n [x]) ++ (replicate_ n xs)


dropEvery_ :: Int -> [a] -> [a]
dropEvery_ n [] = []
dropEvery_ 1 _  = []
dropEvery_ n xs = let nx = length xs - 1
                  in [xs !! (i - 1) | i <- [1..nx], mod i n /= 0]


split_ :: Int -> [a] -> ([a], [a])
split_ _ [] = error "Empty list"
split_ n xs = let first = take n xs
                  last_ = drop n xs
              in (first, last_)


slice_ :: Int -> Int -> [a] -> [a]
slice_ a b = take (b + 1 - a) . (drop a)


rotate_ :: Int -> [a] -> [a]
rotate_ n xs
    | n < 0     = rotate_ (length xs + n) xs
    | otherwise = (drop n xs) ++ (take n xs)


removeElement_ :: Int -> [a] -> [a]
removeElement_ n xs = take n xs ++ drop (n + 1) xs
