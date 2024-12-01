module Main where

import Generator

type Sudoku = [[Int]]

-- Ausgabe eines Sudoku-Feldes in der Konsole
printSudoku :: Sudoku -> IO ()
printSudoku sudoku = do
  putStrLn $ unlines [unwords (map (\n -> if n == 0 then "." else show n) row) | row <- sudoku]

-- Benutzer-Eingabe
getUserInput :: IO (Int, Int, Int)
getUserInput = do
  putStrLn "Gebe die Zeile (0-8) ein:"
  row <- readLn
  putStrLn "Gebe die Spalte (0-8) ein:"
  col <- readLn
  putStrLn "Gebe die Zahl (1-9) ein:"
  num <- readLn
  return (row, col, num)

-- Aktualisiert das Sudoku-Feld mit der Benutzereingabe
updateSudoku :: Sudoku -> IO Sudoku
updateSudoku sudoku = do
  (row, col, num) <- getUserInput
  if isValid sudoku row col num
    then return (put sudoku row col num)
    else do
      putStrLn "Ungültige Eingabe, bitte versuchen es erneut."
      updateSudoku sudoku

-- Hauptspiel-Funktion
playSudoku :: Sudoku -> IO ()
playSudoku sudoku = do
  putStrLn "Aktuelles Sudoku:"
  printSudoku sudoku
  if isSolved sudoku
    then putStrLn "GZ. it's solved haha"
    else do
      updatedSudoku <- updateSudoku sudoku
      playSudoku updatedSudoku

-- Beispiel-Hauptfunktion
main :: IO ()
main = do
  putStrLn "Generiere ein zufälliges Rätsel..."
  generatedSudoku <- generateSudoku
  puzzle <- generatePuzzle generatedSudoku 40  -- 40 Felder werden geleert
  playSudoku puzzle
