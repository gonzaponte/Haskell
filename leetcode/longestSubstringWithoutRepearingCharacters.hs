import Data.List(inits, tails, nub, maximumBy)
import Data.Function(on)
import Utils(fst3, snd3, thd3)

longestSubstringWithoutRepeatingCharacters :: String -> String
longestSubstringWithoutRepeatingCharacters ss = let allsubs        = concat . map inits . tails $ ss
                                                    allDifferent x = x == nub x
                                                    summary        = map (\x-> (allDifferent x, length x, x)) allsubs
                                                in thd3 . maximumBy (compare `on` snd3) . filter fst3 $ summary
