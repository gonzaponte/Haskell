import Data.List(elemIndex)
import Data.Char(digitToInt)
import Data.Maybe

type Vector a = [a]

sumDigits :: Int -> Int
sumDigits = sum . (map digitToInt) . show

luhn :: String -> Bool
luhn xs = let digits   = map digitToInt xs
              check    = last digits
              body     = reverse $ init digits
              nmax     = pred $ length body
              even     = map (sumDigits . (2*) . (body !!)) [0,2..nmax]
              odd      = map                     (body !!)  [1,3..nmax]
              checksum = check + sum even + sum odd
          in checksum `mod` 10 == 0

euclid :: (Integral a) => a -> a -> a
euclid a 0 = a
euclid 0 b = 0
euclid a b
    | a > b = euclid (a - b) b
    | otherwise = euclid a (b - a)


distanceND :: (Floating a) => Vector a -> Vector a -> a
distanceND a b = sqrt . sum . (map (**2)) $ zipWith (-) a b

findMin2 :: (Ord a) => [a] -> (Int, a)
findMin2 xs = let minelem = minimum xs
              in case (elemIndex minelem xs) of Just  i -> (i, minelem)
                                                Nothing -> error ""

findMin :: (Ord a, Integral b) => [a] -> (b, a)
findMin [x]  = (0, x)
findMin (x:xs)
    | x <= minRest = (0, x)
    | otherwise    = (1 + index, minRest)
    where (index, minRest) = findMin xs

closest :: (Floating a, Ord a) => [Vector a] -> [Vector a] -> (Vector a, Vector a)
closest [ ]  _  = error ""
closest  _  [ ] = error ""
closest  xs  ys = let ds = [findMin $ map (distanceND x) ys | x <- xs]
                      (indices, mins) = unzip ds
                      xindex = fst $ findMin mins
                      yindex = indices !! xindex
                  in (xs !! xindex, ys !! yindex)
