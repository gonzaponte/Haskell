import Data.List(splitAt)

addToTuple :: (Num a) => a -> (a, a) -> (a, a)
addToTuple x (l, r) = (x + l, x + r)


findFirstAndLastPositionOfElementInSortedArray :: (Ord a) => [a] -> a -> Maybe (Int, Int)
findFirstAndLastPositionOfElementInSortedArray [] x = Nothing
findFirstAndLastPositionOfElementInSortedArray xs x
  | x == l    = Just (0, nequal)
  | x >  h    = fmap offsetRight searchRight
  | otherwise = fmap offsetLeft  searchLeft
  where len         = length xs
        half        = div len 2
        nequal      = pred . length $ takeWhile (x==) xs
        sndHalf     = snd $ splitAt half xs
        l           = head xs
        h           = head sndHalf
        offsetLeft  = addToTuple              1
        offsetRight = addToTuple $ half + 1 + 1
        searchLeft  = findFirstAndLastPositionOfElementInSortedArray (tail xs) x
        searchRight = findFirstAndLastPositionOfElementInSortedArray sndHalf   x
