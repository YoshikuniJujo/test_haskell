
ccollatz :: Integer -> [Integer]
ccollatz = iterate step

step :: Integer -> Integer
step n = case n `mod` 3 of
	0 -> n `div` 3
	1 -> n * 2 + 1
	2 -> n * 2 + 2
