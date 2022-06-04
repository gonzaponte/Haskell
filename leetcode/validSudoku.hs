import Data.List(nub)
import Data.List(transpose)
import Data.Maybe(isJust)

type SudokuSector = [Maybe Int]
type Sudoku       = [SudokuSector]

row :: Sudoku -> Int -> SudokuSector
row = (!!)

col :: Sudoku -> Int -> SudokuSector
col s c = row (transpose s) c

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


allDifferent :: (Eq a) => [a] -> Bool
allDifferent xs = let unique = nub xs
                  in length unique == length xs

validSector :: SudokuSector -> Bool
validSector = allDifferent . filter isJust

validSudoku :: Sudoku -> Bool
validSudoku s = let rowsOK = map validSector $ map (row s) [0..8]
                    colsOK = map validSector $ map (col s) [0..8]
                    boxsOK = map validSector $ map (box s) [0..8]
                in and $ map and [rowsOK, colsOK, boxsOK]


convertZeros :: [[Int]] -> Sudoku
convertZeros xs = let toMaybe x = if x == 0 then Nothing else Just x
                  in map (map toMaybe) xs
