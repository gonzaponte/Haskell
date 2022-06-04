module Utils
(enumerate
,fst3
,snd3
,thd3
,willOverflowInt
,willUnderflowInt
,argSort
,combinations
,insertAt
,deleteAt
,replaceAt
,allPossibleSubsets
) where

import Data.Function(on)
import Data.List(sortOn)
import Data.List(inits)
import Data.List(tails)


enumerate :: [a] -> [(Int, a)]
enumerate = zip [0..]


fst3 :: (a, b, c) -> a
snd3 :: (a, b, c) -> b
thd3 :: (a, b, c) -> c
fst3 (f, _, _) = f
snd3 (_, s, _) = s
thd3 (_, _, t) = t


willOverflowInt :: String -> Bool
willOverflowInt n
    | ((<) `on` length) n maxInt = False
    | ((>) `on` length) n maxInt = True
    | otherwise                  = n > maxInt
    where maxInt = show (maxBound ::Int)


willUnderflowInt :: String -> Bool
willUnderflowInt n
    | ((<) `on` length) n minInt = False
    | ((>) `on` length) n minInt = True
    | otherwise                  = n > minInt
    where minInt = show (minBound ::Int)


argSort :: (Ord a) => [a] -> [Int]
argSort = map fst . sortOn snd . enumerate


combinations :: [a] -> Int -> [[a]]
combinations [] _ = []
combinations  _ 0 = [[]]
combinations xs@(x:rest) n
    | n >  length xs = []
    | n == length xs = [xs]
    | otherwise      = let withFirst    = map (x:) $ combinations rest (n-1)
                           withoutFirst = combinations rest n
                       in withFirst ++ withoutFirst


insertAt :: [a] -> Int -> a -> [a]
insertAt xs i x = take i xs ++ x : drop i xs


deleteAt :: [a] -> Int -> [a]
deleteAt xs i = take i xs ++ drop (i+1) xs


replaceAt :: [a] -> Int -> a -> [a]
replaceAt xs i x = take i xs ++ x : drop (i+1) xs


allPossibleSubsets :: [a] -> [[a]]
allPossibleSubsets = (concatMap inits) . tails
