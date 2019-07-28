------------------------------------------------------------
-- lists
------------------------------------------------------------
-- head a = a[0]
-- tail a = a[1:]
-- init a = a[:-1]
-- last a = a[-1]

-- length  a = len(a) !!! returns Int, not Num
-- null    a = a == []
-- reverse a = reverse(a)
-- sum     a = sum(a)
-- product a = reduce(operator.mul, a)

-- maximum a = max(a)
-- minimum a = min(a)

-- take a b = b[:a]
-- drop a b = b[a:]
-- elem a b = a in b
-- zip  a b = zip(a, b)
-- (++) a b = [*a, *b]
-- (:)  a b = [a, *b]
-- (!!) a b = a[b]

-- zipWith f a b = map(f, a, b)

-- cycle     a   = while True: for i in a: yield i
-- repeat    a   = while True: yield a
-- replicate a b = [b for i in range(a)]

-- [x + y | x <- [1..5], y <- [2..4], mod (x * y) 2 == 1] =
-- [x + y for x in range(1, 6) for y in range(2, 5) if (x * y) % 2 == 1]

------------------------------------------------------------
-- numbers
------------------------------------------------------------
-- odd  a = a % 2 == 1
-- even a = a % 2 == 0

-- max a b = a if a > b else b
-- min a b = a if a < b else b

-- succ a = a + 1
-- pred a = a - 1

-- div a b = a // b

-- fromIntegral a = a # in Num!

------------------------------------------------------------
-- 2-tuples (aka pairs)
------------------------------------------------------------
-- fst a = a[0]
-- snd a = a[0]

------------------------------------------------------------
-- Syntax
------------------------------------------------------------
-- functionName :: typeArg1 -> typeArg2 -> ... -> typeOut
-- functionName :: (Constraint1, Constraint2, ...) => typeArg1 -> typeArg2 -> ... -> typeOut

-- functionName pattern1 = result1
-- functionName pattern2 = result2
-- functionName global@pattern3 = result3, global = input

-- functionName arg1 arg2 ... =
--     let var1 = something1
--         var2 = something2
--     in expression

-- functionName arg1 arg2 ...
--     | condition1 = result1
--     | condition2 = result2
--     | condition3 = result3
--     | otherwise  = result1
--     where var1 = something1
--           var2 = something2

-- functionName arg1 arg2 ... = case arg* of pattern1 -> output1
--                                           pattern2 -> output2
--                                           pattern3 -> output3

-- functionName arg1 arg2 ... = expression
--     where subFunctionName pattern1 = output1
--           subFunctionName pattern2 = output2
--           subFunctionName pattern3 = output3

-- \x y z ... -> something = lambda x, y, z, ...: something

-- $ f g = f g # with forced evaluation of g

-- (.) f g = lambda *args: f(g(*args))

------------------------------------------------------------
-- functional
------------------------------------------------------------
-- flip      f a b = f     (b, a)
-- map       f a   = map   (f, a)
-- filter    f a   = filter(f, a)
-- takeWhile f a   = for i in a: if f(i): yield i else break
-- foldl     f a b = reduce(f,          b     , initial=a    ) # f = f(acum,  val)
-- foldr     f a b = reduce(f, reversed(b)    , initial=a    ) # f = f( val, acum)
-- foldl1    f a   = reduce(f,          a [1:], initial=a[0] ) # f = f(acum,  val)
-- foldr1    f a   = reduce(f, reversed(a)[1:], initial=a[-1]) # f = f( val, acum)
-- scanl     f a b = accumulate(         [a] + b , f)
-- scanr     f a b = accumulate(reversed([a] + b), f)
-- scanl1    f a   = accumulate(          a      , f)
-- scanr1    f a   = accumulate(reversed( a )    , f)
-- on        f g   = lambda x, y: f(g(x), g(y)) (Data.Function)

quicksort :: (Ord a) => [a] -> [a]
quicksort []     = []
quicksort (x:xs) =
    let smaller_than_x = quicksort (filter (<=x) xs) -- or [i | i <- xs, i <= x]
        bigger_than_x  = quicksort (filter (> x) xs) -- or [i | i <- xs, i >  x]
    in smaller_than_x ++ [x] ++ bigger_than_x

collatz :: (Integral a) => a -> [a]
collatz 1 = [1]
collatz n
    | m == 0 = n : collatz (n `div` 2)
    | m == 1 = n : collatz (n * 3 + 1)
    where m = mod n 2

sum_with_foldl :: (Num a) => [a] -> a
sum_with_foldl xs = foldl (+) 0 xs

map_with_foldr :: (a -> b) -> [a] -> [b]
map_with_foldr f xs = foldr (\x s -> f x : s) [] xs

flatten :: [[a]] -> [a]
flatten xs = foldr (++) [] xs -- or... flatten = foldr (++) []


-- caesarCipher :: Int -> String -> String
-- caesarCipher 0 s = s
-- caesarCipher n s = let asints = map ord s in map chr $ map (+n) asints


data BinaryTree a = Leaf | Tree a (BinaryTree a) (BinaryTree a) deriving (Show, Read, Eq)


value :: BinaryTree a -> a
value (Tree a _ _) = a


left :: BinaryTree a -> BinaryTree a
left  (Tree _ l _) = l


right :: BinaryTree a -> BinaryTree a
right (Tree _ _ r) = r


singletonTree :: a -> BinaryTree a
singletonTree x = Tree x Leaf Leaf


insertTree :: (Ord a) => a -> BinaryTree a -> BinaryTree a
insertTree x Leaf = singletonTree x
insertTree x (Tree v l r)
    | x < v  = Tree v   (insertTree x l) r
    | x > v  = Tree v l (insertTree x r)
    | x == v = Tree v l r


elemTree :: (Ord a) => a -> BinaryTree a -> Bool
elemTree _ Leaf = False
elemTree x (Tree v l r)
    | x == v = True
    | x <  v = elemTree x l
    | x >  v = elemTree x r


treeFromList :: (Ord a) => [a] -> BinaryTree a
treeFromList = foldr insertTree Leaf

treeToList :: (Ord a) => BinaryTree a -> [a]
treeToList Leaf = []
treeToList (Tree v l r) = v : (treeToList l) ++ (treeToList r)
