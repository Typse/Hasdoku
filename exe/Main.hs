module Main where

import Generator
import Solver (solve)

type Sudoku = [[Int]]

-- Ausgabe eines Sudoku-Feldes in der Konsole
printSudoku :: Sudoku -> IO ()
printSudoku sudoku = do
    putStrLn $ unlines $ formatSudoku sudoku

formatSudoku :: Sudoku -> [String]
formatSudoku sudoku = concatMap formatBlock [0, 3, 6]
  where
    formatBlock r =
      [ concatMap (\c -> unwords (map (\n -> if n == 0 then "." else show n) (take 3 (drop c (sudoku !! (r + i))))) ++ " | ") [0, 3, 6] | i <- [0..2]]
      ++ [divider]
    divider = replicate 21 '-'

-- Benutzer-Eingabe
getUserInput :: IO (Int, Int, Int)
getUserInput = do
    putStrLn "Gebe die Eingabe im Format 'Zeile Spalte Zahl' (z.B. '1 2 3') oder '-1' um abzubrechen ein:"
    input <- getLine
    if input == "-1"
        then return (-1, -1, -1)
        else do
            let parts = map read (words input) :: [Int]
            if length parts == 3
                then let [row, col, num] = parts
                     in return (row - 1, col - 1, num)
                else do
                    putStrLn "Ungültiges Format. Bitte erneut versuchen."
                    getUserInput

-- Benutzerwahl für Anzahl der leeren Felder
getPuzzleDifficulty :: IO Int
getPuzzleDifficulty = do
    putStrLn "Wie viele Felder sollen leer sein? (1-64):"
    input <- getLine
    let emptyFields = read input :: Int
    if emptyFields >= 1 && emptyFields <= 64
        then return emptyFields
        else do
            putStrLn "Ungültige Zahl. Bitte einen Wert zwischen 1 und 60 eingeben."
            getPuzzleDifficulty

-- Aktualisiert das Sudoku-Feld mit der Benutzereingabe
updateSudoku :: Sudoku -> Int -> IO (Sudoku, Int)
updateSudoku sudoku errors = do
    (row, col, num) <- getUserInput
    if row == -1 || col == -1 || num == -1
        then do
            let (solvedSudoku, success) = solve sudoku
            if success
                then do
                    putStrLn "\nDas Sudoku wurde geloest!"
                    printSudoku solvedSudoku
                    return (solvedSudoku, errors)
                else do
                    putStrLn "Es wurde eine falsche Zahl platziert. Sudoku ist nicht lösbar."
                    return (sudoku, errors)
        else if isValid sudoku row col num
            then return (put sudoku row col num, errors)
            else do
                putStrLn "\nUngueltige Eingabe, bitte versuchen es erneut.\n"
                let newErrors = errors + 1
                putStrLn $ "Fehleranzahl: \n" ++ show newErrors
                printSudoku sudoku
                updateSudoku sudoku newErrors

-- Hauptspiel-Funktion
playSudoku :: Sudoku -> Int -> IO ()
playSudoku sudoku errors = do
    putStrLn "\nAktuelles Sudoku:\n---------------------"
    printSudoku sudoku
    if isSolved sudoku
        then putStrLn $ "Herzlichen Glückwunsch! Das Sudoku wurde gelöst mit " ++ show errors ++ " Fehler(n)."
        else do
            (updatedSudoku, newErrors) <- updateSudoku sudoku errors
            playSudoku updatedSudoku newErrors

-- Beispiel-Hauptfunktion
main :: IO ()
main = do
    putStrLn "\nGeneriere ein zufaelliges Raetsel..."
    putStrLn "\nMit -1 als Starteingabe loest du sofort das Sudoku."
    emptyFields <- getPuzzleDifficulty
    generatedSudoku <- generateSudoku
    puzzle <- generatePuzzle generatedSudoku emptyFields
    playSudoku puzzle 0