import System.Random

circle :: Int -> Int -> Double
circle t g = fromIntegral (length pins) / fromIntegral t * 4
	where
	pins = filter inCircle $ take t $ randomXY g

inCircle :: (Double, Double) -> Bool
inCircle (x, y) = x ^ 2 + y ^ 2 <= 1

randomXY :: Int -> [(Double, Double)]
randomXY g = map snd $ filter fst $ zip (cycle [True, False]) $ zip doubles $ tail doubles
	where
	doubles = randomRs (-1, 1) $ mkStdGen g
