import System.Console.Terminfo (Point (col))
import System.FilePath (isValid)
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
validInput s c r n =
  let x = put s c r n
   in validColumn x c && validRow x r && validBox x c r

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

-- Valide Box
validBox :: Sudoku -> Int -> Int -> Bool
validBox s col row = validLine (getBox s row col)

getBox :: Sudoku -> Int -> Int -> [Int]
getBox s row col =
  [ s !! r !! c
  | r <- [boxRowStart .. boxRowStart + 2],
    c <- [boxColStart .. boxColStart + 2]
  ]
  where
    boxRowStart = (row `div` 3) * 3
    boxColStart = (col `div` 3) * 3

-- Valide Spalte
validColumn :: Sudoku -> Int -> Bool
validColumn s col = validLine(getColumn s col)

validRow :: Sudoku -> Int -> Bool
validRow s row = validLine(s !! row)

-- Hilffunktion: gibt die Column rückwärts!!!
getColumn :: Sudoku -> Int -> [Int]
getColumn s col = map (\row -> row !! col) s

-- Überprüft ein Array auf valide, nub filter alle duplikate, length gibt die länge aus
validLine :: [Int] -> Bool
validLine xs =
  let nonZero = filter (/= 0) xs
  in length nonZero == length (nub nonZero)

