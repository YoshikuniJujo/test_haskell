squares :: [Int]
squares = map (^ 2) [1 ..]

mins :: [Int]
mins = 0 : map (\n -> (1 +)
	. minimum
	$ (map ((mins !!) . (n -)) $ takeWhile (<= n) squares)) [1 ..]

squares2 :: [[Int]]
squares2 = map (\x -> map (\y -> x ^ 2 + y ^ 2) [x ..]) [1 ..]
