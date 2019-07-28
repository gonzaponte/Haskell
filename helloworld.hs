main = do
    putStrLn "Hello, world!"
    putStrLn "Hey, what's your name?"
    name <- getLine
    putStrLn $ "Hey, hi " ++ name ++ "!!"
