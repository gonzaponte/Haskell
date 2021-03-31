import Data.Function(on)
import Data.List    (transpose, minimumBy)
import Utils        (enumerate, replaceAt)

mergeKSortedLists :: (Ord a) => [[a]] -> [a]
mergeKSortedLists []    = []
mergeKSortedLists [[]]  = []
mergeKSortedLists xss   = let nonNull = filter (not . null) xss
                              first = map head nonNull
                              (i,x) = minimumBy (compare `on` snd) $ enumerate first
                              new   = replaceAt nonNull i $ tail (nonNull !! i)
                          in x : mergeKSortedLists new
