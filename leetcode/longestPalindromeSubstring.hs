import Data.List(inits, tails, nub, maximumBy)
import Data.Function(on)
import Utils(fst3, snd3, thd3)

longestPalindromeSubstring :: String -> String
longestPalindromeSubstring ss = let allsubs        = concat . map inits . tails $ ss
                                    isPalindrome x = x == reverse x
                                    summary        = map (\x-> (isPalindrome x, length x, x)) allsubs
                                in thd3 . maximumBy (compare `on` snd3) . filter fst3 $ summary
