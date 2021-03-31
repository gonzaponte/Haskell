import Utils        (enumerate)
import Data.List    (nub, nubBy)
import Data.Maybe   (isJust, fromJust)
import Data.Function(on)

type Quadruplet a = (a, a, a, a)

sortTuple4 :: (Ord a) => Quadruplet a -> Quadruplet a
sortTuple4 (a, b, c, d)
    | a > b     = sortTuple4 (b, a, c, d)
    | b > c     = sortTuple4 (a, c, b, d)
    | c > d     = sortTuple4 (a, b, d, c)
    | otherwise =            (a, b, c, d)


sumTarget :: (Num a, Eq a) => a -> (Int, a) -> (Int, a) -> (Int, a) -> (Int, a) -> Maybe (Quadruplet a)
sumTarget target (i,a) (j,b) (k,c) (l, d)
    | differentIndices && nullresult = Just (a, b, c, d)
    | otherwise                      = Nothing
    where differentIndices = (4==) . length $ nub [i, j, k, l]
          nullresult   = (a + b + c + d) == target


fourSum :: (Num a, Ord a) => [a] -> a -> [Quadruplet a]
fourSum xs target = let ixs   = enumerate xs
                        sums  = sumTarget target <$> ixs <*> ixs <*> ixs <*> ixs
                        valid = map fromJust $ filter isJust sums
                    in nubBy ((==) `on` sortTuple4) valid
