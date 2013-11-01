
keima :: (Int, Int) -> [(Int, Int)]
keima (x, y) = [(x - 1, y + 2), (x + 1, y + 2)]

keima3 :: [(Int, Int)]
keima3 = do
	k1 <- keima (2, 1)
	k2 <- keima k1
	keima k2
