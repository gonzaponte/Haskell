import Data.List

minmax :: (Ord a) => a -> a -> (a, a)
minmax x y
  | x <= y    = (x, y)
  | otherwise = (y, x)

swapIntoPosition :: (Ord a) => [a] -> a -> [a]
swapIntoPosition [] y = [y]
swapIntoPosition xs@(x:rest) y
    | x <= y    = x : swapIntoPosition rest y
    | otherwise = y : xs

insertionSort :: (Ord a) => [a] -> [a]
insertionSort = foldl swapIntoPosition []

insertInSorted :: (Ord a) => [a] -> a -> [a]
insertInSorted []           y = [y]
insertInSorted xs@(x:xtail) y
    | y < x     = y : xs
    | otherwise = x : (insertInSorted xtail y)

selectionSort :: (Ord a) => [a] -> [a]
selectionSort = foldl insertInSorted []

mergePair :: (Ord a) => [a] -> [a] -> [a]
mergePair [] ys = ys
mergePair xs [] = xs
mergePair xs@(x:xt) ys@(y:yt)
    | x <= y    = x : mergePair xt ys
    | otherwise = y : mergePair xs yt

splitInHalf :: [a] -> ([a], [a])
splitInHalf xs = splitAt (genericLength xs `div` 2) xs

mergeSort :: (Ord a) => [a] -> [a]
mergeSort [ ] = [ ]
mergeSort [x] = [x]
mergeSort xs = let (f, s) = splitInHalf xs
               in mergePair (mergeSort f) (mergeSort s)

data Heap a = Leaf
            | Node {getLeft  :: Heap a,
                    getValue ::      a,
                    getRight :: Heap a}
            deriving (Show, Eq)

buildHeap :: (Ord a) => [a] -> Heap a
buildHeap []     = Leaf
buildHeap (x:xs) = let (left, right) = splitInHalf xs
                   in  Node (buildHeap left) x (buildHeap right)

newValue :: Heap a -> a -> Heap a
newValue (Node left old right) new = Node left new right

siftDown :: (Ord a) => Heap a -> Heap a
siftDown parent@(Node Leaf value Leaf) = parent

siftDown (Node Leaf value right)
    | rValue > value = Node Leaf rValue (siftDown (newValue right value))
    where rValue = getValue right

siftDown (Node left value Leaf)
    | lValue > value = Node (siftDown (newValue left value)) lValue Leaf
    where lValue = getValue left

siftDown (Node left value right)
    | (lValue > value) && (lValue >  rValue) = Node (siftDown (newValue left value)) lValue (siftDown right)
    | (rValue > value) && (rValue >= lValue) = Node (siftDown left)                  rValue (siftDown (newValue right value))
    | otherwise                              = Node (siftDown left)                   value (siftDown right)
    where lValue = getValue left
          rValue = getValue right

makeHeapMax :: (Ord a) => Heap a -> Heap a
makeHeapMax = siftDown

{-
exchangeValues :: Heap a -> (a, Heap a)
exchangeValues h = (getValue h, h)

heapSort :: [a] -> [a]
heapSort [] = []
heapSort xs = let heap = makeHeapMax (buildHeap xs)
              in xs : heapSort heap 
-}

middle :: [a] -> a
middle xs = xs !! ((genericLength xs) `div` 2)

quickSort :: (Ord a) => [a] -> [a]
quickSort [ ]    = [ ]
quickSort [x]    = [x]
quickSort xs     = let index         = (genericLength xs) `div` 2
                       pivot         = xs !! index
                       (left, right) = partition (< pivot) (delete pivot xs)
                   in  (quickSort left) ++ [pivot] ++ (quickSort $ right)



----------------------------------------------
----------------------------------------------

testVector1   = [ 1,  4, -1,  2,  3,  0,  6]
testVector2   = [ 1,  4,  4,  4,  3,  1,  2]
testVector3   = take 7 $ repeat 5
testVector4   = [5..12]
sortedVector1 = [-1,  0,  1,  2,  3,  4,  6]
sortedVector2 = [ 1,  1,  2,  3,  4,  4,  4]
sortedVector3 = testVector3
sortedVector4 = testVector4

testMethod name method = do
    let sorted1 = method testVector1
        sorted2 = method testVector2
        sorted3 = method testVector3
        sorted4 = method testVector4
        test1   = sorted1 == sortedVector1
        test2   = sorted2 == sortedVector2
        test3   = sorted3 == sortedVector3
        test4   = sorted4 == sortedVector4
    putStrLn $ replicate 20 '='
    putStrLn   name
    putStrLn $ replicate 20 '='
    putStrLn $ "Test vector 1: " ++ (show testVector1)
    putStrLn $ "After sorting: " ++ (show     sorted1)
    putStrLn $ "Passed       : " ++ (show       test1)
    putStrLn $ "Test vector 2: " ++ (show testVector2)
    putStrLn $ "After sorting: " ++ (show     sorted2)
    putStrLn $ "Passed       : " ++ (show       test2)
    putStrLn $ "Test vector 3: " ++ (show testVector3)
    putStrLn $ "After sorting: " ++ (show     sorted3)
    putStrLn $ "Passed       : " ++ (show       test3)
    putStrLn $ "Test vector 4: " ++ (show testVector4)
    putStrLn $ "After sorting: " ++ (show     sorted4)
    putStrLn $ "Passed       : " ++ (show       test4)
    putStrLn   ""
main = do
    testMethod "Insertion sort" insertionSort
    testMethod "Selection sort" selectionSort
    testMethod "Merge     sort"     mergeSort
    testMethod "Quick     sort"     quickSort