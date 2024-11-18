type Sudoku = [[int]]

generate :: Sudoku
generate =
    [(row, col) | row <- [1..9], col <-[1..9]]
