main :: IO ()
main = print . sum $ points
	[(1, 2), (4, 6), (3, 7), (3, 4), (5, 2), (10, 0), (3, 6), (9, 0), (2, 3)]
	(7, 3, 2)

pnts :: Int -> Int -> [(Int, Int)] -> [Int]
pnts _ _ [] = []
pnts x y ((10, 0) : ps) = x + y + 10 : pnts y 10 ps
pnts x y ((z, w) : ps)
	| z + w == 10 = x + 10 : pnts z w ps
	| z + w < 10 = z + w : pnts z w ps
	| otherwise = error "bad score"

points :: [(Int, Int)] -> (Int, Int, Int) -> [Int]
points ps (x, y, z) = reverse $ x + y + z : pnts y x (reverse ps)
