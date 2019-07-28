import System.Random
import Data.List

square = map (**2)

main = do
    putStr "How many randoms? "
    rng      <- getStdGen
    nRandoms <- fmap (round . read) getLine
    let randoms_xy        = randomRs (-1, 1) rng :: [Float]
        (randoms_x, rest) = splitAt nRandoms randoms_xy
        (randoms_y, _   ) = splitAt nRandoms rest
        radii             = zipWith (+) (square randoms_x) (square randoms_y)
        n_in              = genericLength $ filter (<1) radii
        pi_estimation     =       n_in  * 4 / (fromIntegral nRandoms)
        pi_error          = (sqrt n_in) * 4 / (fromIntegral nRandoms)
    putStr   "My estimation of pi is... "
    putStr   $ show pi_estimation
    putStr   $ " +- "
    putStr   $ show pi_error
    putStrLn "!!!"

