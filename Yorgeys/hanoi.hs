type Peg  = String
type Move = (Peg, Peg)

hanoi3 :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi3 0 a b c = []
hanoi3 1 a b c = [(a, c)]
hanoi3 2 a b c = [(a, b), (a, c), (b, c)]
hanoi3 n a b c = (hanoi3 (n - 1) a c b) ++ ((a, c) : (hanoi3 (n-1) b a c))

hanoi4 :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoi4 0 a b c d = []
hanoi4 1 a b c d = [(a, d)]
hanoi4 2 a b c d = [(a, b),  (a, d), (b, d)]
hanoi4 3 a b c d = [(a, b),  (a, c), (a, d), (c, d), (b, d)]
hanoi4 n a b c d = let fhalf = div n 2
                       shalf = n - fhalf
                   in (hanoi4 fhalf a c d b) ++ (hanoi3 shalf a c d) ++ (hanoi4 fhalf b a c d)
