import Data.List(lookup)

numberMap = [('2', "abc" )
            ,('3', "def" )
            ,('4', "ghi" )
            ,('5', "jkl" )
            ,('6', "mno" )
            ,('7', "pqrs")
            ,('8', "tuv" )
            ,('9', "wxyz")
            ]

letterCombinationsOfAPhoneNumber :: String -> [String]
letterCombinationsOfAPhoneNumber ""     = [""]
letterCombinationsOfAPhoneNumber (x:xs) = let rest = letterCombinationsOfAPhoneNumber xs
                                          in case lookup x numberMap of
                                              Nothing      -> [""]
                                              Just letters -> (:) <$> letters <*> rest
