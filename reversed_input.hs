getInput = do
    putStrLn "What's your input?"
    input <- getLine
    return input


loop = do
    input <- getInput
    if null input
        then    putStrLn "Ok, goodbye"
        else do putStrLn $ reverse input
                loop

main = do
    putStrLn "Hi there! This program outputs whatever you input"
    putStrLn "backwards until you input a blank line. Isn't it cool?"
    loop
