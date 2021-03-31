import Data.Char(isDigit, digitToInt)
import Utils(willOverflowInt, willUnderflowInt)

signFromChar :: Char -> Int
signFromChar '-' = -1
signFromChar '+' =  1
signFromChar n
    | isDigit n = 1
    | otherwise = 0


stringToInteger :: String -> Maybe Int
stringToInteger "" = Nothing
stringToInteger s
    | null s    = Nothing
    | null ss   = Nothing
    | not  ok   = Nothing
    | otherwise = Just result
    where ss       = dropWhile (==' ') s
          fstChar  = head ss
          sndChar  = head $ tail ss
          ok       = isDigit fstChar || (fstChar `elem` "+-" && length ss > 1 && isDigit sndChar)
          sign     = signFromChar $ head ss
          unsigned = if sign == -1 then tail ss else ss
          digits   = map digitToInt . reverse . takeWhile isDigit $ unsigned
          powers   = map (10^) [0..]
          result   = (sign*) $ foldr (+) 0 $ zipWith (*) digits powers
