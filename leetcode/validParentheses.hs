
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
