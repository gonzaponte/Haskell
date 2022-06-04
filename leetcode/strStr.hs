import Data.List(tails)
import Utils(enumerate)

strStr :: String -> String -> Maybe Int
strStr str  "" = Nothing
strStr  "" sub = Nothing
strStr str sub = let takeSub = take $ length sub
                     eSubs   = enumerate $ map takeSub $ tails str
                     matches = filter ((==sub) . snd) eSubs
                 in if null matches then Nothing else Just $ fst $ head matches
