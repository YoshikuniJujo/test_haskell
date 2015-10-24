
cons :: Integer -> Integer -> Integer
cons x y = 2 ^ x * 3 ^ y

unpower :: Integer -> Integer -> Integer
unpower n x
	| x `mod` n /= 0 = 0
	| otherwise = 1 + unpower n (x `div` n)

uncons :: Integer -> (Integer, Integer)
uncons xy = (unpower 2 xy, unpower 3 xy)
