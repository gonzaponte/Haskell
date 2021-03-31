import Utils(enumerate)

twoSum :: (Num a, Eq a) => [a] -> a -> (Int, Int)
twoSum xs s = let ixs  = enumerate xs
                  sums = [(i, j) | (i,x) <- ixs, (j,y) <- ixs, i /= j, x+y==s]
              in  head $ sums
