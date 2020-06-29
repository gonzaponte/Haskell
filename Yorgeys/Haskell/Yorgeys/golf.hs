module Golf where

import Data.List

takeEvery :: [a] -> Int -> [a]
takeEvery [] _ = []
takeEvery  _ 0 = []
takeEvery xs 1 = xs
takeEvery xs n = [x | (i, x) <- zip [1..] xs, 0 == mod i n]

skips :: [a] -> [[a]]
skips xs = map (takeEvery xs) [1..length xs]

splitEvery :: [a] -> Int -> [[a]]
splitEvery [] _ = []
splitEvery xs 0 = [xs]
splitEvery xs n = let (taken, rest) = splitAt n xs
                  in taken : splitEvery xs n


localMaxima :: [Integer] -> [Integer]
localMaxima xs
  | l < 3 = []
  | otherwise     = [ m | (_, m, _) <- filter (\(x, y, z) -> y > x && y > z) $ zip3 (take (l-2) xs) (take (l-1) $ drop 1 xs) (drop 2 xs)]
  where l = length xs

histogram :: [Integer] -> String
histogram xs = let range    = [0..9]
                   count x  = genericLength $ filter (==x) xs
                   counts   = map count range
                   maxn     = maximum counts
                   stars n  = replicate n '*'
                   spaces n = replicate (maxn - n) ' '
                   columns  = [(show i) ++ "=" ++ stars n ++ spaces n | (i, n) <- zip range counts]
               in  (intercalate "\n" . reverse $ transpose columns) ++ "\n"

