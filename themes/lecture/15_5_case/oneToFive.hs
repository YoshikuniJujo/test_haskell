oneToFive :: Integer -> Integer
oneToFive n = case n `mod` 5 of
	0 -> 5
	r -> r
