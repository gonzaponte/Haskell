findOperand :: (Num a) => String -> (a -> a -> a)
findOperand "+"   = (+)
findOperand "-"   = (-)
findOperand "*"   = (*)
findOperand other = error $ "Invalid operand: " ++ other

evaluate :: (Num a, Read a) => [a] -> String -> [a]
evaluate stack value
  | isOperand = operand hh h : rest
  | otherwise = number       : stack
  where isOperand   = value `elem` ["+", "-", "*"]
        operand     = findOperand value
        number      = read value
        (h:hh:rest) = stack

solveRPN :: (Num a, Read a) => String -> a
solveRPN s = head $ foldl evaluate [] $ words s
