import System.Random
import Data.List (genericLength)


insertAt_ :: Int -> a -> [a] -> [a]
insertAt_ n x xs = let (a, b) = splitAt n xs
                   in a ++ [x] ++ b


deleteAt_ :: Int -> [a] -> [a]
deleteAt_ n xs = let (a, b) = splitAt n xs
                 in a ++ (tail b)


range_ :: Int -> Int -> [Int]
range_ a b
    | b <  a = reverse $ range_ b a
    | b == a = [a]
    | b >  a = (:) a $ range_ (a + 1) b


selectRand_ :: Int -> [a] -> IO [a]
selectRand_ n xs = do g <- newStdGen
                      return $ take n $ map (xs !!) $ randomRs (0, length xs - 1) g


integerRand_ :: Int -> Int -> Int -> IO [Int]
integerRand_ n a b = selectRand_ n [a..b]


pop :: Int -> [a] -> (a, [a])
pop n xs
    | n >= l    = error "Index out or range"
    | n <  0    = pop (l + n) xs
    | otherwise = let item = xs !! n
                      rest = deleteAt_ n xs
                  in (item, rest)
    where l = length xs

popRand_ :: [a] -> IO (a, [a])
popRand_ [] = error "Empty list"
popRand_ xs = do g <- newStdGen
                 let (index, newGen) = randomR (0, length xs - 1) g
                 return $ pop index xs

permutationRand_ :: [a] -> IO [a]
permutationRand_ [] = return []
permutationRand_ xs = do (item, rest) <- popRand_ xs
                         permutedRest <- permutationRand_ rest
                         return $ item:permutedRest


flatten :: [[a]] -> [a]
flatten = foldr (++) []


generatePermutations :: [a] -> [[a]]
generatePermutations []  = []
generatePermutations [x] = [[x]]
generatePermutations  xs = flatten [let (x, rest) = pop i xs
                                        sub = generatePermutations rest
                                    in (map (x:) sub) | i<- [0..(length xs - 1)]]


takePermutations :: Int -> [a] -> [[a]]
takePermutations 0 _   = [[]]
takePermutations _ []  =  []
takePermutations 1 [x] = [[x]]
takePermutations n xs  = flatten [let (x, rest) = pop i xs
                                      sub = takePermutations (n-1) rest
                                  in (map (x:) sub) | i<- [0..(length xs - 1)]]


indexTake :: [Int] -> [a] -> [a]
indexTake indices list = map (list !!) indices


sort :: (Ord a) => [a] -> [a]
sort []     = []
sort (x:xs) = let smaller_than_x = sort $ filter (<=x) xs -- or [i | i <- xs, i <= x]
                  bigger_than_x  = sort $ filter (> x) xs -- or [i | i <- xs, i >  x]
              in smaller_than_x ++ [x] ++ bigger_than_x


-- Sort a list of tuples according to the first element
sortTuples :: (Ord a) => [(a, b)] -> [(a, b)]
sortTuples []     = []
sortTuples (x:xs) = let lessThan y     = fst y <= fst x
                        moreThan y     = fst y >  fst x
                        smaller_than_x = sortTuples $ filter lessThan xs -- or [i | i <- xs, i <= x]
                        bigger_than_x  = sortTuples $ filter moreThan xs -- or [i | i <- xs, i >  x]
                    in smaller_than_x ++ [x] ++ bigger_than_x


argSort :: (Ord a) => [a] -> [Int]
argSort xs = map snd $ sortTuples $ zip xs [0..]


sortBy :: (Ord b) => (a -> b) -> [a] -> [a]
sortBy _ [] = []
sortBy f xs = map snd $ sortTuples $ zip (map f xs) xs

sortByLengths :: [[a]] -> [[a]]
sortByLengths = sortBy length
