data Matryoshka = Nest Matryoshka | Term deriving Show

count :: Matryoshka -> Int
count (Nest m) = 1 + count m
count _ = 1

create :: Int -> Matryoshka
create n | n < 2 = Term
create n = Nest . create $ n - 1
