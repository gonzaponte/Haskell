
normalChars = [('I',    1)
              ,('V',    5)
              ,('X',   10)
              ,('L',   50)
              ,('C',  100)
              ,('D',  500)
              ,('M', 1000)
              ]

specialChars = [("IV", 4 )
               ,("IX", 9 )
               ,("XL", 40)
               ,("XC", 90)
               ,("CD", 400)
               ,("CM", 900)
               ]

romanToInteger :: String -> Maybe Int
romanToInteger      ""    = Just 0
romanToInteger (  x:""  ) = lookup x normalChars
romanToInteger (x:y:rest) = let special     = lookup (x:y:"") specialChars
                                normal      = lookup x normalChars
                                headNumber  = case special of Just a  -> Just a
                                                              Nothing -> normal
                                remaining   = case special of Just a  -> romanToInteger    rest
                                                              Nothing -> romanToInteger (y:rest)
                            in (+) <$> headNumber <*> remaining
