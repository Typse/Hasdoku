module Main where

import Generator
import Solver (solve)

type Sudoku = [[Int]]

-- Ausgabe eines Sudoku-Feldes in der Konsole
printSudoku :: Sudoku -> IO ()
printSudoku sudoku = do
    putStrLn $ unlines $ formatSudoku sudoku
--  putStrLn $ unlines [unwords (map (\n -> if n == 0 then "." else show n) row) | row <- sudoku]

formatSudoku :: Sudoku -> [String]
formatSudoku sudoku = concatMap formatBlock [0, 3, 6] -- concatMap wendet /formatBlock/ in jeder 3. Zeile / Spalte an
  where
  -- Hilfsfunktion für formatSudoku
    formatBlock r =
      [ concatMap (\c -> unwords (map (\n-> if n == 0 then "." else show n) (take 3 (drop c (sudoku !! (r + i))))) ++ " | ") [0, 3, 6] | i <- [0..2]]
      ++ [divider]
    divider = replicate 21 '-'  -- Linie zwischen den 3x3-Blöcken

-- Benutzer-Eingabe
getUserInput :: IO (Int, Int, Int)
getUserInput = do
        putStrLn "Gebe die Zeile (1-9) ein:"
        row <- readLn
        if row == -1
            then return (-1, -1, -1)
            else do
                putStrLn "Gebe die Spalte (1-9) ein:"
                col <- readLn
                putStrLn "Gebe die Zahl (1-9) ein:"
                num <- readLn
                return (row-1, col-1, num)

-- Aktualisiert das Sudoku-Feld mit der Benutzereingabe
updateSudoku :: Sudoku -> IO Sudoku
updateSudoku sudoku = do
    (row, col, num) <- getUserInput
    if row == -1 || col == -1 || num == -1
        then do
            let (solvedSudoku, success) = solve sudoku
            if success
                then do
                    putStrLn "Das Sudoku wurde geloest!"
                    printSudoku solvedSudoku
                    return solvedSudoku
                else do
                    putStrLn "hallo ich bin nutzlos! Weil die generierten sudokus immer loesbar sind"
                    return sudoku
        else if isValid sudoku row col num
            then return (put sudoku row col num)
            else do
                putStrLn "Ungueltige Eingabe, bitte versuchen es erneut."
                printSudoku sudoku
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
  putStrLn "Generiere ein zufaelliges Raetsel..."
  putStrLn "Mit -1 als Starteingabe loest du sofort das Sudoku"
  generatedSudoku <- generateSudoku
  puzzle <- generatePuzzle generatedSudoku 40  -- 40 Felder werden geleert
  playSudoku puzzle
