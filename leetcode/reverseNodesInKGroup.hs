import Data.List(splitAt)

reverseNodesInKGroup :: Int -> [a] -> [a]
reverseNodesInKGroup n xs = let (f, b) = splitAt n xs
                                front = reverse f
                                back  = reverseNodesInKGroup n b
                            in front ++ back
