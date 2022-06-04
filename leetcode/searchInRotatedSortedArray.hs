import Data.List(splitAt)

searchInRotatedSortedArray :: (Ord a) => [a] -> a -> Maybe Int
searchInRotatedSortedArray [] x = Nothing
searchInRotatedSortedArray xs x
  | x == l    = Just 0
  | x == h    = Just half
  | x == r    = Just (len - 1)
  | otherwise = if (rStrictly) then (if (x >  h) && (x <= r) then searchRight else searchLeft )
                               else (if (x >= l) && (x <  h) then searchLeft  else searchRight)
  where len          = length xs
        l            = head   xs
        r            = last   xs
        half         = div len 2
        (xl, (h:xr)) = splitAt half xs
        searchLeft   = fmap (     1+) $ searchInRotatedSortedArray (tail xl) x
        searchRight  = fmap (half+1+) $ searchInRotatedSortedArray (init xr) x
        rStrictly    = h <= r
