import Data.List(splitAt)

swapNodesInPairs :: [a] -> [a]
swapNodesInPairs []  = []
swapNodesInPairs [x] = [x]
swapNodesInPairs xs = let ([a,b], c) = splitAt 2 xs
                      in b:a: swapNodesInPairs c
