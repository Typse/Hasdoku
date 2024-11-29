-- geht nicht. Hab auch nicht hinbekommen, es über cabal zu installieren
-- import System.Random


type Sudoku = [[Int]]

-- value + seed -> random value
seed :: Int
seed = 2
fakeRandom :: Int -> Int
fakeRandom val = (seed * val + 3) `mod` 9

sudSize :: Int
sudSize = 9

emtpySud :: Sudoku
emtpySud = replicate sudSize (replicate sudSize 0)

-- Sudoku + Position der neuen Zahl + neue Zahl -> Sudoku mit neuer Zahl
setCell :: Sudoku -> (Int, Int) -> Int -> Sudoku
setCell sud (blockId, cellId) val =
        take blockId sud ++ [take cellId row ++ [val] ++ drop (cellId + 1) row] ++ drop (blockId + 1) sud
    where
        row = sud !! blockId

-- Sudoku + Anzahl an Zahlen löschen -> neues Sudoku
removeCells :: Sudoku -> Int -> Sudoku
removeCells sud 0  = sud
removeCells sud n = do
        let x = fakeRandom n
        let y = fakeRandom n
        if sud !! x !! y == 0
            then removeCells sud n -- Leeres Feld / Feld mit 0 überspringen
            else removeCells (setCell sud (x, y) 0) (n - 1)

-- Soduku + Position des Checks + geprüfte Zahl -> True wenn valid, sonst False
isValid :: Sudoku -> (Int, Int) -> Int -> Bool
isValid sud (blockId, cellId) val =
        notElem val (sud !! blockId)
        && notElem val [sud !! i !! cellId | i <- [0 .. sudSize -1]]
        && notElem val (getBlock sud blockId)

getBlock :: Sudoku -> Int -> [Int]
getBlock sud blockId =
    sud !! blockId

fillSudoko :: Sudoku -> Sudoku
fillSudoko sud = sud
