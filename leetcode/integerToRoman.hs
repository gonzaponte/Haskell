import Data.Char(digitToInt)

type Roman = String

charToRoman :: Char -> Int -> Roman
charToRoman c 4 = case c of 'I' -> "IV"
                            'X' -> "XL"
                            'C' -> "CD"
charToRoman c 9 = case c of 'I' -> "IX"
                            'X' -> "XC"
                            'C' -> "CM"
charToRoman c n
    | n < 5     = replicate n c
    | otherwise = case c of 'I' -> "V" ++ replicate (n-5) c
                            'X' -> "L" ++ replicate (n-5) c
                            'C' -> "D" ++ replicate (n-5) c

integerToRoman :: Int -> Roman
integerToRoman n = let sint = show n
                       rDigits = reverse $ take (length sint) "IXCM"
                       iToR    = concat . zipWith charToRoman rDigits . map digitToInt
                   in iToR sint
