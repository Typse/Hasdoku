module Generator (
  emptySudoku,
  generateSudoku,
  generatePuzzle,
  isSolved,
  isValid,
  put
) where

import System.Random (randomRIO)
import Data.List (transpose, nub)

type Sudoku = [[Int]]

-- Leeres Sudoku
emptySudoku :: Sudoku
emptySudoku = replicate 9 (replicate 9 0)

-- Zufällige Generierung eines vollständigen Sudokus
generateSudoku :: IO Sudoku
generateSudoku = fillSudoku emptySudoku 0 0

-- Generiert ein rätsel durch gezieltes Leeren von Feldern
generatePuzzle :: Sudoku -> Int -> IO Sudoku
generatePuzzle sudoku holes = do
  puzzle <- removeValues sudoku holes
  return puzzle

-- Hilfsfunktion zum Entfernen von Werten aus einem S-FELD
removeValues :: Sudoku -> Int -> IO Sudoku
removeValues sudoku 0 = return sudoku
removeValues sudoku n = do
  row <- randomRIO (0, 8)
  col <- randomRIO (0, 8)
  if (sudoku !! row) !! col == 0
    then removeValues sudoku n  -- Falls das Feld schon leer ist, andere posi wählen
    else do
      let newSudoku = put sudoku row col 0
      removeValues newSudoku (n - 1)

-- Hilfsfunktion zum Befüllen des Sudokus
fillSudoku :: Sudoku -> Int -> Int -> IO Sudoku
fillSudoku sudoku row col
  | row == 9 = return sudoku  -- Basisfall: Alle Zeilen sind durchlaufen, Sudoku ist vollständig
  | col == 9 = fillSudoku sudoku (row + 1) 0  -- Spaltenende erreicht, zur nächsten Zeile springen
  | otherwise = do
      if (sudoku !! row) !! col /= 0
        then fillSudoku sudoku row (col + 1)  -- Wenn Feld bereits belegt ist, zur nächsten Spalte
        else do
          nums <- shuffle [1..9]  -- Zufällige Reihenfolge der Zahlen von 1 bis 9
          tryNumbers nums sudoku row col

-- Probiert die Zahlen in zufälliger Reihenfolge aus, um das Feld zu füllen
tryNumbers :: [Int] -> Sudoku -> Int -> Int -> IO Sudoku
tryNumbers [] sudoku _ _ = return sudoku
tryNumbers (n:ns) sudoku row col
  | isValid sudoku row col n = do  -- Prüft, ob die Zahl n in der Position (row, col) gültig ist
      let newSudoku = put sudoku row col n
      result <- fillSudoku newSudoku row (col + 1)
      if isSolved result
        then return result
        else tryNumbers ns sudoku row col
  | otherwise = tryNumbers ns sudoku row col

-- Hilfsfunktion für zufälliges mischen der Liste
shuffle :: [a] -> IO [a]
shuffle [] = return []
shuffle xs = do
  idx <- randomRIO (0, length xs - 1)
  let (left, (y:right)) = splitAt idx xs
  rest <- shuffle (left ++ right)
  return (y : rest)

-- Setzt eine Zahl in das Sudoku ein
put :: Sudoku -> Int -> Int -> Int -> Sudoku
put sudoku row col n =
  take row sudoku ++
  [take col (sudoku !! row) ++ [n] ++ drop (col + 1) (sudoku !! row)] ++
  drop (row + 1) sudoku

-- Prüft, ob eine Zahl an eine bestimmte Position gesetzt werden kann
isValid :: Sudoku -> Int -> Int -> Int -> Bool
isValid sudoku row col n =
  notElem n (sudoku !! row) &&
  notElem n (map (!! col) sudoku) &&
  notElem n (getBox sudoku row col)

-- Hilfsfunktion zum Abrufen des entsprechenden 3x3-Blocks
getBox :: Sudoku -> Int -> Int -> [Int]
getBox sudoku row col =
  [sudoku !! r !! c | r <- [boxRowStart .. boxRowStart + 2], c <- [boxColStart .. boxColStart + 2]]
  where
    boxRowStart = (row `div` 3) * 3
    boxColStart = (col `div` 3) * 3

-- Prüft, ob das Sudoku vollständig gelöst ist (keine Nullen mehr)
isSolved :: Sudoku -> Bool
isSolved sudoku = all (all (/= 0)) sudoku
