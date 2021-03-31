import Utils(willOverflowInt, willUnderflowInt)

reverseInteger :: Int -> Maybe Int
reverseInteger n
    | positive && willOverflowInt  reversed = Nothing
    | negative && willUnderflowInt reversed = Nothing
    | otherwise                             = Just $ read reversed
    where strn     = show n
          negative = head strn == '-'
          positive = not negative
          reversed = if negative
                     then '-' : reverse (tail strn)
                     else reverse strn
