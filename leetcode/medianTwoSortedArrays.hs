import Data.List(genericLength)

mergeKeepingOrder [] ys = ys
mergeKeepingOrder xs [] = xs
mergeKeepingOrder xall@(x:xs) yall@(y:ys)
    | y < x     = y : mergeKeepingOrder xall ys
    | otherwise = x : mergeKeepingOrder xs   yall


medianTwoSortedArrays :: (Ord a, Floating a) => [a] -> [a] -> a
medianTwoSortedArrays ns ms
    | odd totalLength = head $ sndHalf
    | otherwise       = (/2) . sum . take 2 $ sndHalf
    where merged      = mergeKeepingOrder ns ms
          totalLength = genericLength merged
          half        = (totalLength - 1) `div` 2
          sndHalf     = drop half merged
