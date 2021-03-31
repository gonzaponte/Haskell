-- NOT FINISHED

zigZagConversion :: String -> Int -> String
zigZagConversion s nrows = let spaces = repeat ' '
                               (chunk, rest)  = splitAt (2*nrows - 2) s
                           in [c | (i, c) <- enumerate s]

s !! 0 : take nrows ' '
s !! 1 : take (nrows - 2) $ repeat ' ' : s !! 4 : take 1 ' '
s !! 2 : take (nrows - 3) $ repeat ' ' : s !! 3 : take 2 ' '
