import System.Environment(getArgs)

opMap = [("+"  , (+)  )
        ,("-"  , (-)  )
        ,("*"  , (*)  )
        ,("/"  , (/)  )
        ,("**" , (**) )
        ]

addStack :: [String] -> String -> [String]
addStack st x = case lookup x opMap of
                  Just op -> let (newst, args) = splitAt (length st - 2) st
                                 (arg1:arg2:[]) = map read args
                             in newst ++ [show $ op arg1 arg2]
                  Nothing -> st ++ [x]

rpnSolver :: (Read a, Floating a) => String -> a
rpnSolver s = let tokens = words s
                  result = foldl addStack [] tokens
              in read $ head result


main = do
    result <- fmap rpnSolver $ fmap head getArgs
    print result


-- Example ./rpnSolver "10 4 3 + 2 * -"
