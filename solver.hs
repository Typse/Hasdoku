type Sudoku = [[Int]]

-- Ein eindeutig lösbares Sudoku-Rätsel
sudoku :: Sudoku
sudoku =
--  c1 c2
  [ [5, 3, 0, 0, 7, 0, 0, 0, 0],    --r1
    [6, 0, 0, 1, 9, 5, 0, 0, 0],    --r2
    [0, 9, 8, 0, 0, 0, 0, 6, 0],
    [8, 0, 0, 0, 6, 0, 0, 0, 3],
    [4, 0, 0, 8, 0, 3, 0, 0, 1],
    [7, 0, 0, 0, 2, 0, 0, 0, 6],
    [0, 6, 0, 0, 0, 0, 2, 8, 0],
    [0, 0, 0, 4, 1, 9, 0, 0, 5],
    [0, 0, 0, 0, 8, 0, 0, 7, 9]
  ]

--          Array    col    row    num
isValid :: Sudoku -> Int -> Int -> Int -> Bool
isValid s c r n =
  let x = put s c r n
   in validColum x && validRow x && validBox x

--     Array     col    row    num    ArrayResult
put :: Sudoku -> Int -> Int -> Int -> Sudoku
put s c r n =   take r s ++
                [replaceIndex (drop r (take (r+1) s)) c n] ++   -- (drop (r-1)(take r s)) = (s !! r)
                drop (r+1) s
