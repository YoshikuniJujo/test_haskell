sum3N :: Integer -> Integer
sum3N n = sum $ map (* 3) [0 .. n]

sum3N5 :: Integer -> Integer
sum3N5 n = sum . filter ((/= 0) . (`mod` 5)) $ map (* 3) [0 .. n]
