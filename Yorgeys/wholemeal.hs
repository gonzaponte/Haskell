fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x:xs)
  | even x = (x - 2) * fun1 xs
  | otherwise = fun1 xs

fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n
  | even n = n + fun2 (n `div` 2)
  | otherwise = fun2 (3 * n + 1)


fun1' = product . map (pred . pred) . filter even
fun2' = let next n = if even n then div n 2 else 3*n + 1
        in  sum . filter even . takeWhile (/= 1) . iterate next


