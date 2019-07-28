boolXor True  = not
boolXor False = id

xor :: [Bool] -> Bool
xor = foldl boolXor False

map' :: (a -> b) -> [a] -> [b]
map' f = foldr ((:) . f) []

myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f base xs = foldr (flip f) base (reverse xs)
