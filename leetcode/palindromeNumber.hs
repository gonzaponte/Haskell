palindromeNumber :: (Show a) => a -> Bool
palindromeNumber s = let sn = show s
                     in sn == reverse sn
