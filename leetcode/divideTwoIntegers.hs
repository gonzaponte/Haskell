import Data.Bits(xor)
import Data.Function(on)

divideTwoIntegers :: Int -> Int -> Int
divideTwoIntegers a b
  | absa >= absb = sign + divideTwoIntegers diff b
  | otherwise    = 0
  where absa = abs a
        absb = abs b
        sign = if (xor `on` (< 0)) a b then -1 else 1
        diff = if a<0 then absb - absa else absa - absb
