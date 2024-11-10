import System.Console.Terminfo (Point (col))
import System.FilePath (isValid)
import Data.List

type Sudoku = [[Int]]

-- Ein eindeutig lösbares Sudoku-Rätsel
sudokuExample :: Sudoku
sudokuExample =
  --  c1 c2
  [ [5, 3, 0, 0, 7, 0, 0, 0, 0], -- r1
    [6, 0, 0, 1, 9, 5, 0, 0, 0], -- r2
    [0, 9, 8, 0, 0, 0, 0, 6, 0],
    [8, 0, 0, 0, 6, 0, 0, 0, 3],
    [4, 0, 0, 8, 0, 3, 0, 0, 1],
    [7, 0, 0, 0, 2, 0, 0, 0, 6],
    [0, 6, 0, 0, 0, 0, 2, 8, 0],
    [0, 0, 0, 4, 1, 9, 0, 0, 5],
    [0, 0, 0, 0, 8, 0, 0, 7, 9]
  ]

-- --             Array    col    row    num
-- validInput :: Sudoku -> Int -> Int -> Int -> Bool
-- validInput s c r n =
--   let x = put s c r n
--    in validColumn x c && validRow x r && validBox x c r

-- Fügt eine Zahl in ein Sudoku Feld ein
--     Array     col    row    num    ArrayResult
put :: Sudoku -> Int -> Int -> Int -> Sudoku
put s col row num =
  take row s
    ++ [replaceIndex (s !! row) col num]
    ++ drop (row + 1) s

-- Hilfsfunktion
replaceIndex :: [Int] -> Int -> Int -> [Int]
replaceIndex s c n =
  take c s
    ++ [n]
    ++ drop (c + 1) s

-- Valide Spalte
validColumn :: Sudoku -> Int -> Bool
validColumn s col = validLine(getColumn s col)

validRow :: Sudoku -> Int -> Bool
validRow s row = validLine(s !! row)

-- Hilffunktion: gibt die Column rückwärts!!!
getColumn :: Sudoku -> Int -> [Int]
getColumn s col = map (!! col) s

-- Überprüft ein Array auf valide, nub filter alle duplikate, length gibt die länge aus
validLine :: [Int] -> Bool
validLine xs =
  let nonZero = filter (/= 0) xs
  in length nonZero == length (nub nonZero)

