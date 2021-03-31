import Utils        (enumerate)
import Data.List    (nub, nubBy)
import Data.Maybe   (isJust, fromJust)
import Data.Function(on)

type Triplet a = (a, a, a)

sortTuple3 :: (Ord a) => Triplet a -> Triplet a
sortTuple3 (a, b, c)
    | a > b     = sortTuple3 (b, a, c)
    | b > c     = sortTuple3 (a, c, b)
    | otherwise =            (a, b, c)

sum0 :: (Num a, Eq a) => (Int, a) -> (Int, a) -> (Int, a) -> Maybe (Triplet a)
sum0 (i,a) (j,b) (k,c)
    | differentIndices && nullresult = Just (a, b, c)
    | otherwise                      = Nothing
    where differentIndices = (3==) . length $ nub [i, j, k]
          nullresult   = (a + b + c) == 0

threeSum :: (Num a, Ord a) => [a] -> [Triplet a]
threeSum xs = let ixs   = enumerate xs
                  sums  = sum0 <$> ixs <*> ixs <*> ixs
                  valid = map fromJust . filter isJust $ sums
              in nubBy ((==) `on` sortTuple3) valid
