{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

sort :: Ord a => [a] -> [a]
sort = mergesort . map (: [])

mergesort :: Ord a => [[a]] -> [a]
mergesort [] = []
mergesort [xs] = xs
mergesort xss = mergesort $ mergePairs xss

mergePairs :: Ord a => [[a]] ->[[a]]
mergePairs [] = []
mergePairs [xs] = [xs]
mergePairs (xs : ys : xss) = merge xs ys : mergePairs xss

merge :: Ord a => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge (x : xs) (y : ys)
	| x <= y = x : merge xs (y : ys)
	| otherwise = y : merge (x : xs) ys
