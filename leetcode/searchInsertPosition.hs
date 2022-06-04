import Utils(enumerate)
import Data.List(dropWhile)

searchInsertPosition :: (Ord a) => [a] -> a -> Either Int Int
searchInsertPosition [] x = Right 0
searchInsertPosition xs x
  | n == 0      = Right $ length xs
  | fValue == x = Left  fIndex
  | otherwise   = Right fIndex
  where greater          = dropWhile ((<x) . snd) $ enumerate xs
        n                = length greater
        (fIndex, fValue) = head greater
