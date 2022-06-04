import Data.Function(on)
import Data.List(sortBy)
import Utils(allPossibleSubsets)


twin :: Char -> Char
twin '(' = ')'
twin '[' = ']'
twin '{' = '}'
twin  _  = ' '

processChar :: String -> Char -> String
processChar  "" c = c : ""
processChar acc c = if twin (last acc) == c then init acc else acc ++ [c]

validParentheses :: String -> Bool
validParentheses = null . foldl processChar ""


longestValidParentheses :: String -> Int
longestValidParentheses s = let subs   = allPossibleSubsets  s
                                valid  = filter validParentheses subs
                                sorted = sortBy (compare `on` length) valid
                            in length $ last sorted
