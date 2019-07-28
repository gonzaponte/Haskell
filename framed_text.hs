import Data.List

frameWord :: String -> Int -> String
frameWord word nmax = let wordWidth = length word
                          nSpaces   = nmax - wordWidth
                      in "* " ++ word ++ (replicate nSpaces ' ') ++ " *"
                   

frameText :: String -> String
frameText s = let ws     = words s
                  nmax   = maximum $ map length ws
                  frame  = flip frameWord nmax
                  border = frameWord (replicate nmax '*') nmax
              in intercalate "\n" $ [border] ++ (map frame ws) ++ [border]

main = do
    putStrLn "Give me some text"
    line <- getLine
    putStrLn $ frameText line