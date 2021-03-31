import Data.Function(on)
import Data.List    (minimumBy)
import Utils        (combinations)

threeSumClosest :: (Num a, Ord a) => [a] -> a -> a
threeSumClosest xs target = let sums     = map sum $ combinations xs 3
                                distance = abs . (target - )
                            in minimumBy (compare `on` distance) sums
