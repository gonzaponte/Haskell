toDigits :: Integer -> [Integer]
toDigits n
  |          n < 0 = []
  | headDigits > 0 = (toDigits headDigits) ++ [lastDigit]
  | otherwise      = [lastDigit]
  where headDigits = div n 10
        lastDigit  = mod n 10


doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther [only] = [only]
doubleEveryOther (f:s:rem) = f : (2*s) : (doubleEveryOther rem)


addDigits :: Integer -> Integer
addDigits n = sum (toDigits n)

validateCard :: Integer -> Bool
validateCard n =
    let digitSum = sum (map addDigits (doubleEveryOther (reverse (toDigits n))))
    in 0 == (mod digitSum 10)

validateCard2 :: Integer -> Bool
validateCard2 n =
    let digitSum = sum $ map addDigits $ doubleEveryOther $ reverse $ toDigits n
    in 0 == (mod digitSum 10)

