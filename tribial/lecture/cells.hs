data Cell = Cell Char Int deriving Show

column :: Cell -> Char
column (Cell c _) = c

row :: Cell -> Int
row (Cell _ r) = r

cell1 :: Cell
cell1 = Cell 'A' 8
