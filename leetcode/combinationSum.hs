import Utils        (allPossibleSubsets)
import Data.List    (nub, sort, permutations)
import Data.Maybe   (isJust, fromJust)
import Data.Function(on)


combinationSumII :: (Num a, Ord a) => [a] -> a -> [[a]]
combinationSumII xs target = let leq = filter (<=target) xs
                                 ps  = concatMap allPossibleSubsets . permutations $ leq
                             in nub . map sort . filter ((==target) . sum) $ ps


combinationSum :: (Num a, Ord a) => [a] -> a -> [[a]]
combinationSum  xs target
  | target < 0 = []
  | otherwise  = nub . map sort . filter ((==target) . sum) $ both
  where taking0 = combinationSumII xs  target
        take1 x = combinationSum   xs (target - x)
        taking1 = concatMap take1 xs
        both    = taking0 ++ taking1
