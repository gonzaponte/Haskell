main = do
    input <- getContents
    putStrLn "So this should be printed"
    putStrLn "Now we will use the value:"
    putStrLn $ "This is my input:" ++ input
    return ()