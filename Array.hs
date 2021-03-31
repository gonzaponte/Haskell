infixr 4 :.
data Array a = EmptyArray | a :. (Array a) deriving (Read, Eq)

instance (Show a) => Show (Array a) where
    show EmptyArray        = "Empty"
    show (x :. EmptyArray) = show x
    show (x :. xs)  = show x ++ " " ++ show xs


len :: (Num b) => Array a -> b
len EmptyArray = 0
len (x :. xs)  = 1 + len xs

sumArray :: (Num a) => Array a -> a
sumArray EmptyArray = 0
sumArray (x :. xs)  = x + sumArray xs

modulo :: (Floating a) => Array a -> a
modulo = sqrt . sumArray . (**.2)

shiftLeft :: Array a -> Array a
shiftLeft EmptyArray = EmptyArray
shiftLeft (x :. xs)  = append xs x

shiftRight :: Array a -> Array a
shiftRight EmptyArray = EmptyArray
shiftRight xs@(x :. EmptyArray)  = xs
shiftRight (x0 :. x1 :. EmptyArray) = x1 :. x0 :.
    | len xs == 1 =


infixl 5 +.
(+.) :: (Num a) => Array a -> Array a -> Array a
EmptyArray +. EmptyArray = EmptyArray
xs +. ys
    | wrongShape = error "These arrays have different shapes"
    | otherwise  = let (x0:.x1) = xs
                       (y0:.y1) = ys
                   in (x0 + y0) :. x1 +. y1
    where wrongShape = len xs == len ys

infixl 5 -.
(-.) :: (Num a) => Array a -> Array a -> Array a
EmptyArray -. EmptyArray = EmptyArray
xs -. ys
    | wrongShape = error "These arrays have different shapes"
    | otherwise  = let (x0:.x1) = xs
                       (y0:.y1) = ys
                   in (x0 - y0) :. x1 -. y1
    where wrongShape = len xs == len ys

infixl 7 *.
(*.) :: (Num a) => Array a -> Array a -> Array a
EmptyArray *. EmptyArray = EmptyArray
xs *. ys
    | wrongShape = error "These arrays have different shapes"
    | otherwise  = let (x0:.x1) = xs
                       (y0:.y1) = ys
                   in (x0 * y0) :. x1 *. y1
    where wrongShape = len xs == len ys

infixl 7 /.
(/.) :: (Fractional a) => Array a -> Array a -> Array a
EmptyArray /. EmptyArray = EmptyArray
xs /. ys
    | wrongShape = error "These arrays have different shapes"
    | otherwise  = let (x0:.x1) = xs
                       (y0:.y1) = ys
                   in (x0 / y0) :. x1 /. y1
    where wrongShape = len xs == len ys

infixl 8 **.
(**.) :: (Floating a) => Array a -> a -> Array a
EmptyArray **. _ = EmptyArray
(x:.xs) **. exponent = x**exponent :. xs **. exponent
