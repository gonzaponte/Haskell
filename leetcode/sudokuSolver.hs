import Prelude hiding (take)

import Data.List( nub
                , transpose
                )

import Data.Set( take
               , elems
               , fromList
               , difference
               )

import Data.Maybe( catMaybes
                 ,   isJust
                 , fromJust
                 )

import Data.Monoid(    Any
                  , getAny
                  )

import Utils( enumerate
            , replaceAt
            )

type SudokuSector = [Maybe Int]
type Sudoku       = [SudokuSector]

row :: Sudoku -> Int -> SudokuSector
row = (!!)

col :: Sudoku -> Int -> SudokuSector
col s = row $ transpose s

cell :: Sudoku -> Int -> Int -> Maybe Int
cell sdk i j = (sdk !! i) !! j

box :: Sudoku -> Int -> SudokuSector
box s 0 = [(s !! r) !! c | r <- [0..2], c <- [0..2]]
box s 1 = [(s !! r) !! c | r <- [0..2], c <- [3..5]]
box s 2 = [(s !! r) !! c | r <- [0..2], c <- [6..8]]
box s 3 = [(s !! r) !! c | r <- [3..5], c <- [0..2]]
box s 4 = [(s !! r) !! c | r <- [3..5], c <- [3..5]]
box s 5 = [(s !! r) !! c | r <- [3..5], c <- [6..8]]
box s 6 = [(s !! r) !! c | r <- [6..8], c <- [0..2]]
box s 7 = [(s !! r) !! c | r <- [6..8], c <- [3..5]]
box s 8 = [(s !! r) !! c | r <- [6..8], c <- [6..8]]
box s _ = error "Invalid index"


boxIndices :: Int -> [(Int, Int)]
boxIndices 0 = (,) <$> [0..2] <*> [0..2]
boxIndices 1 = (,) <$> [0..2] <*> [3..5]
boxIndices 2 = (,) <$> [0..2] <*> [6..8]
boxIndices 3 = (,) <$> [3..5] <*> [0..2]
boxIndices 4 = (,) <$> [3..5] <*> [3..5]
boxIndices 5 = (,) <$> [3..5] <*> [6..8]
boxIndices 6 = (,) <$> [6..8] <*> [0..2]
boxIndices 7 = (,) <$> [6..8] <*> [3..5]
boxIndices 8 = (,) <$> [6..8] <*> [6..8]
boxIndices _ = error "Invalid index"


boxFromIndices :: Int -> Int -> Int
boxFromIndices i j = let r = div i 3
                         c = div j 3
                     in 3*r + c

replaceSdkAt :: Sudoku -> Int -> Int -> Int -> Sudoku
replaceSdkAt sdk i j value = let oldrow = row sdk i
                                 newrow = replaceAt oldrow j (Just value)
                             in replaceAt sdk i newrow


possibleValues :: Sudoku -> (Int, Int) -> [Int]
possibleValues sdk (i, j)
  | isJust value = []
  | otherwise    = elems valid
  where value     = cell sdk i j
        rowValues = catMaybes $ row sdk i
        colValues = catMaybes $ col sdk j
        boxValues = catMaybes . box sdk $ boxFromIndices i j
        allValues = fromList [1..9]
        valid     = difference allValues $ fromList (rowValues ++ colValues ++ boxValues)


solveCell :: Sudoku -> (Int, Int) -> Sudoku
solveCell sdk (i, j)
  | single vs = replaceSdkAt sdk i j value
  | otherwise = sdk
  where vs     = possibleValues sdk (i, j)
        single = (==1) . length
        value  = head vs


solveNumberBox :: Sudoku -> Int -> Int -> Sudoku
solveNumberBox sdk n b
  | justOne   = replaceSdkAt sdk i j n
  | otherwise = sdk
  where availableCells = filter (elem n . possibleValues sdk) $ boxIndices b
        justOne        = length availableCells == 1
        (i, j)         = head availableCells


solveNumberRow :: Sudoku -> Int -> Int -> Sudoku
solveNumberRow sdk n r
  | justOne   = replaceSdkAt sdk i j n
  | otherwise = sdk
  where availableCells = filter (elem n . possibleValues sdk) $ (,) <$> [r] <*> [0..8]
        justOne        = length availableCells == 1
        (i, j)         = head availableCells


solveNumberCol :: Sudoku -> Int -> Int -> Sudoku
solveNumberCol sdk n c
  | justOne   = replaceSdkAt sdk i j n
  | otherwise = sdk
  where availableCells = filter (elem n . possibleValues sdk) $ (,) <$> [0..8] <*> [c]
        justOne        = length availableCells == 1
        (i, j)         = head availableCells


solveBox :: Sudoku -> Int -> Sudoku
solveBox sdk b = foldr (\n s -> solveNumberBox s n b) sdk [1..9]


solveRow :: Sudoku -> Int -> Sudoku
solveRow sdk r = foldr (\n s -> solveNumberRow s n r) sdk [1..9]


solveCol :: Sudoku -> Int -> Sudoku
solveCol sdk c = foldr (\n s -> solveNumberCol s n c) sdk [1..9]


sudokuSolver :: Sudoku -> Sudoku
sudokuSolver sdk = let indices  = [0..8]
                       pairs    = (,) <$> indices <*> indices
                       sdk1     = foldr (flip solveCell) sdk  pairs
                       sdk2     = foldr (flip solveBox ) sdk1 indices
                       sdk3     = foldr (flip solveRow ) sdk2 indices
                       sdk4     = foldr (flip solveCol ) sdk3 indices
                       sdkfinal = sdk4
                       solved   = (81==) . length . catMaybes . concat $ sdkfinal
                       stuck    = null $ diff sdk sdkfinal
                   in if solved || stuck then sdkfinal else sudokuSolver sdkfinal


convertZeros :: [[Int]] -> Sudoku
convertZeros xs = let toMaybe x = if x == 0 then Nothing else Just x
                  in map (map toMaybe) xs


fromString :: [[String]] -> Sudoku
fromString []     = []
fromString (s:ss) = let f si = if si == "." then Nothing else Just $ read si
                    in map f s : fromString ss

toString :: Sudoku -> [[String]]
toString []     = []
toString (s:ss) = let f = maybe "." show
                  in map f s : toString ss

diff :: Sudoku -> Sudoku -> [(Int, Int, Maybe Int, Maybe Int)]
diff a b = let adata  = [(i, j, v) | (i, r) <- enumerate a, (j, v) <- enumerate r]
               bdata  = [(i, j, v) | (i, r) <- enumerate b, (j, v) <- enumerate r]
               zipped = zipWith (\(i, j, a) (_, _, b) -> (i, j, a, b)) adata bdata
               different Nothing Nothing = False
               different Nothing _ = True
               different _ Nothing = True
               different (Just x) (Just y) = x/=y
           in filter (\(_, _, a, b) -> different a b) zipped

printSdk :: Sudoku -> IO ()
printSdk = mapM_ print . toString

nCorrect :: Sudoku -> Int
nCorrect = length . catMaybes . concat


sudoku = fromString [ ["5","3",".",".","7",".",".",".","."]
                    , ["6",".",".","1","9","5",".",".","."]
                    , [".","9","8",".",".",".",".","6","."]
                    , ["8",".",".",".","6",".",".",".","3"]
                    , ["4",".",".","8",".","3",".",".","1"]
                    , ["7",".",".",".","2",".",".",".","6"]
                    , [".","6",".",".",".",".","2","8","."]
                    , [".",".",".","4","1","9",".",".","5"]
                    , [".",".",".",".","8",".",".","7","9"]
                    ]

solved = sudokuSolver sudoku
