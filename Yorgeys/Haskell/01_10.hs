last_ :: [a] -> a
last_ []     = error "Empty list"
last_ [x]    = x
last_ (x:xs) = last_ xs


last_but_one_ :: [a] -> a
last_but_one_ []     = error "Empty list"
last_but_one_ [x]    = error "List too short"
last_but_one_ [x,y]  = x
last_but_one_ (x:xs) = last_but_one_ xs


getElemAt_ :: Int -> [a] -> a
getElemAt_ n l
    | n >= n0   = error "Index out of range"
    | n < 0     = getElemAt_ (n0 + n) l
    | n == 0    = head l
    | otherwise = getElemAt_ (n - 1) (tail l)
    where n0 = length l

length_ :: (Num b) => [a] -> b
length_ []     = 0
length_ (x:xs) = 1 + length_ xs


reverse_ :: [a] -> [a]
reverse_ [] = []
reverse_ xs = last xs : reverse_ (init xs)


isPalindrome_ :: (Eq a) => [a] -> Bool
isPalindrome_ []  = True
isPalindrome_ [x] = True
isPalindrome_ xs  = (head xs == last xs) && (isPalindrome_ . tail . init) xs


flatten_ :: [[a]] -> [a]
flatten_ = foldr (++) []


compress_ :: (Eq a) => [a] -> [a]
compress_ []     = []
compress_ (x:xs) = x : compress_ (dropWhile (==x) xs)


pack_ :: (Eq a) => [a] -> [[a]]
pack_ [] = []
pack_ x0@(x:xs) = let (eq, diff) = span (== x) x0
                  in eq : pack_ diff


count_duplicated_ :: (Eq a) => [a] -> [(Int, a)]
count_duplicated_ [] = []
count_duplicated_ xs = [(length p, head p) | p <- pack_ xs]
