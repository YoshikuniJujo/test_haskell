productOdd3 :: Integer -> Integer
productOdd3 n = product . filter ((/= 0) . (`mod` 3)) $ map ((+ 1) . (* 2)) [0 .. n]
