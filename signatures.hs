
fl :: (a -> b -> b) -> b -> [a] -> b
fl f y xs = (foldl1 (.) $ map f xs) y


fr :: (a -> b -> b) -> b -> [a] -> b
fr f y xs = (foldr1 (.) $ map f xs) y

gl :: (a -> b -> b) -> b -> [a] -> b
gl f y [x] = f x y
gl f y  xs = let (x:rest) = xs
                 new = f x y
             in gl f new rest

gr :: (a -> b -> b) -> b -> [a] -> b
gr f y [x] = f x y
gr f y  xs = let (x:rest) = xs
                 new = f x y
             in gl f new rest



f :: Int -> String -> String
f n = concat . replicate n
