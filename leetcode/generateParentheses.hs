import Data.List(nub)

generateParentheses :: Int -> [String]
generateParentheses 0 = [""]
generateParentheses 1 = ["()"]
generateParentheses n = let rest    = generateParentheses $ n-1
                            enclose =  (++")") . ('(':)
                            open    = map enclose rest
                            closed1 = map ("()"++) rest
                            closed2 = map (++"()") rest
                        in nub $ open ++ closed1 ++ closed2
