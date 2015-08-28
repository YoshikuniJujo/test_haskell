import Control.Arrow

data Matrix a = Matrix [[a]] deriving Show

pops :: [a] -> [(a, [a])]
pops [] = []
pops (x : xs) = (x, xs) : map (second (x :)) (pops xs)

cofactors :: Matrix a -> [(a, Matrix a)]
cofactors (Matrix xss) = map (head *** Matrix . map tail) $ pops xss

determinant :: Num a => Matrix a -> a
determinant (Matrix [[x]]) = x
determinant m = sum . zipWith (*) (cycle [1, -1]) .
	map (\(n, m) -> n * determinant m) $ cofactors m
