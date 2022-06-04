import System.Environment(getArgs)
import System.Random(split, RandomGen, randomRs, getStdGen, newStdGen)
import Data.Function(on)
import Data.List    (genericLength)

mod2 = (+) `on` (**2)

generateXYCircle :: (RandomGen g) => g -> Double -> ([Double], [Double])
generateXYCircle rng rmax = let (rngX, rngY) = split rng
                                randomsX     = randomRs (-rmax, rmax) rngX
                                randomsY     = randomRs (-rmax, rmax) rngY
                                radii        = zipWith mod2 randomsX randomsY
                                xyr          = zip3 randomsX randomsY radii
                                (xs, ys, _)  = unzip3 $ filter (\(_,_,r) -> r <= rmax) xyr
                            in (xs, ys)

generateRandomDirections2pi :: (RandomGen g) => g -> ([Double], [Double])
generateRandomDirections2pi rng = let (rngTheta, rngPhi) = split rng
                                      randomsCosTheta = randomRs (0, 1   ) rngTheta
                                      randomsTheta    = map acos randomsCosTheta
                                      randomsPhi      = randomRs (0, 2*pi) rngPhi
                                  in (randomsTheta, randomsPhi)

main = do
    rng0 <- getStdGen
    rng1 <- newStdGen
    args <- getArgs
    let readArg  = read . (args !!)
        nRandoms =                 round $ readArg 0 -- :: Int
        sourceR  =                         readArg 1 -- :: Double
        targetR  =                         readArg 2 -- :: Double
        targetZ  =                         readArg 3 -- :: Double
        sourceX  = if length args > 4 then readArg 4 else 0-- :: Double else 0 :: Double
        sourceY  = if length args > 5 then readArg 5 else 0-- :: Double else 0 :: Double

        (    x0s,   y0s) = generateXYCircle rng0 sourceR
        (theta0s, phi0s) = generateRandomDirections2pi rng1

        x1s = zipWith (\t p-> targetZ * tan t * cos p) theta0s phi0s
        y1s = zipWith (\x p-> x * tan p) x1s phi0s
        xs  = map (sourceX+) $ zipWith (+) x0s x1s
        ys  = map (sourceY+) $ zipWith (+) y0s y1s
        rs  = take nRandoms $ zipWith mod2 xs ys

        nIn      = genericLength $ filter (<targetR**2) rs
        nTotal   = fromIntegral nRandoms
        fraction = nIn / nTotal / 2

    putStrLn $ "The fraction of solid angle is " ++ show fraction
    putStrLn $ "The total       solid angle is " ++ show (fraction * 4 * pi) ++ " sr"
