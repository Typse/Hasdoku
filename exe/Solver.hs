module Solver (
solve
) where

import Data.List

type Sudoku = [[Int]]

-- Ein eindeutig lösbares Sudoku-Rätsel
sudokuExample :: Sudoku
sudokuExample =
  --  c0 c1
  [ [5, 3, 0, 0, 7, 0, 0, 0, 0], -- r0
    [6, 0, 0, 1, 9, 5, 0, 0, 0], -- r1
    [0, 9, 8, 0, 0, 0, 0, 6, 0],
    [8, 0, 0, 0, 6, 0, 0, 0, 3],
    [4, 0, 0, 8, 0, 3, 0, 0, 1],
    [7, 0, 0, 0, 2, 0, 0, 0, 6],
    [0, 6, 0, 0, 0, 0, 2, 8, 0],
    [0, 0, 0, 4, 1, 9, 0, 0, 5],
    [0, 0, 0, 0, 8, 0, 0, 7, 9]
  ]

--             Array    col    row    num
validInput :: Sudoku -> Int -> Int -> Int -> Bool
validInput sudoku column row num =
  let x = put sudoku column row num
   in validColumn x column && validRow x row && validBox x column row

-- Fügt eine Zahl in ein Sudoku Feld ein
--     Array     col    row    num    ArrayResult
put :: Sudoku -> Int -> Int -> Int -> Sudoku
put sudoku col row num =
  take row sudoku
    ++ [replaceIndex (sudoku !! row) col num]
    ++ drop (row + 1) sudoku

-- Hilfsfunktion
replaceIndex :: [Int] -> Int -> Int -> [Int]
replaceIndex sudoku column num =
  take column sudoku
    ++ [num]
    ++ drop (column + 1) sudoku

-- Valide Box
validBox :: Sudoku -> Int -> Int -> Bool
validBox sudoku col row = validLine (getBox sudoku row col)

getBox :: Sudoku -> Int -> Int -> [Int]
getBox sudoku row col =
  [ sudoku !! r !! c
    | r <- [boxRowStart .. boxRowStart + 2],
      c <- [boxColStart .. boxColStart + 2]
  ]
  where
    boxRowStart = (row `div` 3) * 3
    boxColStart = (col `div` 3) * 3

-- Valide Spalte
validColumn :: Sudoku -> Int -> Bool
validColumn sudoku col = validLine (getColumn sudoku col)

validRow :: Sudoku -> Int -> Bool
validRow sudoku row = validLine (sudoku !! row)

-- Hilffunktion: gibt die Column rückwärts!!!
getColumn :: Sudoku -> Int -> [Int]
getColumn sudoku col = map (\row -> row !! col) sudoku

-- Überprüft ein Array auf valide, nub filter alle duplikate, length gibt die länge aus
validLine :: [Int] -> Bool
validLine xs =
  let nonZero = filter (/= 0) xs
   in length nonZero == length (nub nonZero)

-- Feld    column  row   Zahl in dem Feld
getIndex :: Sudoku -> Int -> Int -> Int
getIndex sudoku column row = getColumn sudoku column !! row

-- Jetzt kommt der eigentliche Solver
solve :: Sudoku -> (Sudoku, Bool)
solve sudoku = solveSudokuHelper sudoku 0 0

--                    Feld      column  row   gelöstes Feld
solveSudokuHelper :: Sudoku -> Int -> Int -> (Sudoku, Bool)
solveSudokuHelper sudoku column row
  | row == 9 = (sudoku, True)
  | column == 9 = solveSudokuHelper sudoku 0 (row + 1)
  | getIndex sudoku column row /= 0 = solveSudokuHelper sudoku (column + 1) row
  | otherwise =
      let (sudoku', success) = solveSudokuHelperForloop sudoku column row 1
       in if success then (sudoku', True) else (sudoku, False)

-- For loop for trying out nums from 1 to 9
--                          Feld      column  row    Num    Feld
solveSudokuHelperForloop :: Sudoku -> Int -> Int -> Int -> (Sudoku, Bool)
solveSudokuHelperForloop sudoku column row num
  | num == 10 = (sudoku, False)
  | validInput sudoku column row num =
      let (next_sudoku, success) = solveSudokuHelper (put sudoku column row num) (column + 1) row
       in if success
            then (next_sudoku, True)
            else solveSudokuHelperForloop sudoku column row (num + 1)
  | otherwise = solveSudokuHelperForloop sudoku column row (num + 1)
