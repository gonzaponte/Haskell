import Data.List

sieveSundaram :: Integer -> [Integer]
sieveSundaram n =  let exclude = sievable n
                   in map (\x -> 2*x + 1) . filter (`notElem` exclude) $ [1..n]

sievable n = nub $ sort [ x + y + 2*x*y | x <- [1..n], y <- [x..n], x + y + 2*x*y <= n]
