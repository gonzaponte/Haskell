import Data.List(tails)
import Data.List(permutations)
import Utils(enumerate)

findSubStr :: String -> String -> [Int]
findSubStr str  "" = []
findSubStr  "" sub = []
findSubStr str sub = let takeSub = take $ length sub
                         eSubs   = enumerate $ map takeSub $ tails str
                         matches = filter ((==sub) . snd) eSubs
                     in map fst matches


substringWithConcatenationOfAllWords :: String -> [String] -> [Int]
substringWithConcatenationOfAllWords str tokens = let subs = map concat $ permutations tokens
                                                  in concatMap (findSubStr str) subs
