type IntList = [Int]

addTwoNumbers' :: Int -> IntList -> IntList -> IntList
addTwoNumbers' n [] []
    | n == 0    = [ ]
    | otherwise = [n]
addTwoNumbers' n (i:is) (j:js) = let s      = i + j + n
                                     little = s `mod` 10
                                     big    = s `div` 10
                                 in little : addTwoNumbers' big is js

addTwoNumbers :: IntList -> IntList -> wIntList
addTwoNumbers = addTwoNumbers' 0
