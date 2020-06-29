isPrime :: Integer -> Bool
isPrime 0 = False
isPrime 1 = False
isPrime 2 = True
isPrime 3 = True
isPrime x
    | even x    = False
    | otherwise = (== 0) . length $ filter (\i -> (mod x i) == 0) [3,5..x-1]


gcd_ :: (Integral a) => a -> a -> a
gcd_ 0 y = y
gcd_ x 0 = x
gcd_ x y = gcd_ y $ mod x y


areCoprimes :: (Integral a) => a -> a -> Bool
areCoprimes x y = (==1) $ gcd_ x y


totienPhi :: (Integral a) => a -> Int
totienPhi m = length $ filter (\x -> x) $ map (areCoprimes m) [1..m-1]
