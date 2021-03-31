import Data.List(transpose, nub)

longestCommonPrefix :: [String] -> String
longestCommonPrefix ss
    | null result = ""
    | otherwise   = head $ transpose result
    where result = takeWhile ((==1) . length . nub) . transpose $ ss
