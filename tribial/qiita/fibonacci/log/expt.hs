expt :: Integer -> Integer -> Integer
expt a n = exptIter3 a n 1

exptIter, exptIter2, exptIter3 :: Integer -> Integer -> Integer -> Integer
exptIter _ n r | n < 1 = r
exptIter a n r = exptIter a (n - 1) (r * a)

exptIter2 _ n r | n < 1 = r
exptIter2 a n r
	| even n = exptIter2 (a ^ 2) (n `div` 2) r
	| otherwise = exptIter2 a (n - 1) (r * a)

exptIter3 _ n | n < 1 = id
exptIter3 a n
	| even n = exptIter3 (a ^ 2) (n `div` 2)
	| otherwise = exptIter3 a (n - 1) . (* a)
