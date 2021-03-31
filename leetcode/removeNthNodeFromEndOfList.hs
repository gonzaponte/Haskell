import Data.List(delete)

removeNthNodeFromEndOfList :: Int -> [a] -> [a]
removeNthNodeFromEndOfList i xs = let n      = length xs
                                      before = take (n - i    ) xs
                                      after  = drop (n - i + 1) xs
                                  in before ++ after
