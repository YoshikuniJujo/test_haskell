import System.Random

points :: Int -> [(Double, Double)]
points g_ = zip xs ys
	where
	xs = randomRs (-1, 1) g
	ys = randomRs (-1, 1) g'
	(g, g') = split $ mkStdGen g_

inCircle :: (Double, Double) -> Bool
inCircle (x, y) = x ^ 2 + y ^ 2 <= 1

inCirclePoints :: Int -> Int -> [(Double, Double)]
inCirclePoints g n = filter inCircle . take n $ points g

guessPi :: Int -> Int -> Double
guessPi g n = 4 * fromIntegral (length $ inCirclePoints g n) / fromIntegral n
