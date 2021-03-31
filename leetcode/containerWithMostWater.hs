import Utils(enumerate)

area :: (Int, Int) ->  (Int, Int) -> Int
area (i, x) (j, y) = abs (j-i) * min x y

containerWithMostWater :: [Int] -> Int
containerWithMostWater xs = let exs = enumerate xs
                            in maximum $ area <$> exs <*> exs
