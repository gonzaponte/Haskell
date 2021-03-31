flatseq :: [[a]] -> [a]
flatseq = foldr (++) []

data NormMode = First | Second | SumBoth | Diff  deriving (Show, Read)
data Interval = Closed | Open  deriving (Show, Read)

relative_difference :: Fractional a => NormMode -> a -> a -> a
relative_difference First   x y = (x - y) /  x
relative_difference Second  x y = (x - y) /      y
relative_difference SumBoth x y = (x - y) / (x + y)
relative_difference Diff    x y = (x - y) / (x - y)

in_range :: Ord a => Interval -> Interval -> [a] -> a -> a -> [Bool]
in_range Closed Closed xs xmin xmax = [(x >= xmin) && (x <= xmax) | x <- xs]
in_range Open   Open   xs xmin xmax = [(x >  xmin) && (x <  xmax) | x <- xs]
in_range Closed Open   xs xmin xmax = [(x >= xmin) && (x <  xmax) | x <- xs]
in_range Open   Closed xs xmin xmax = [(x >  xmin) && (x <= xmax) | x <- xs]

average :: Fractional a => [a] -> a
average xs = let s = sum xs
                 n = fromIntegral $ length xs
             in s/n

weightedAverage :: Fractional a => [a] -> [a] -> a
weightedAverage xs ws = let s = sum $ zipWith (*) xs ws
                            n = sum ws
                        in s/n

weightedVar :: Floating a => [a] -> [a] -> a
weightedVar xs ws = let m = weightedAverage xs ws
                        d = map (**2) $ map (subtract m) xs
                        s = sum $ zipWith (*) d ws
                        n = sum ws
                    in s/n

weightedStd :: Floating a => [a] -> [a] -> a
weightedStd xs ws = let var = weightedVar xs ws
                    in var**0.5
