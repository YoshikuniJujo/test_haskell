import System.Random

circle :: Double -> Int -> Int -> Double
circle r t g = fromIntegral (length pins) / fromIntegral t * (2 * r) ^ 2
	where
	pins = filter (inCircle r) $ take t $ randomXY (- r, r) g

inCircle :: Double -> (Double, Double) -> Bool
inCircle r (x, y) = x ^ 2 + y ^ 2 <= r ^ 2

randomXY :: (Double, Double) -> Int -> [(Double, Double)]
randomXY r g = zip (randomX r g) (randomY r g)

randomX :: (Double, Double) -> Int -> [Double]
randomX r g = map snd $ filter fst $ zip (cycle [True, False]) (randomDoubles r g)

randomY :: (Double, Double) -> Int -> [Double]
randomY r g = map snd $ filter fst $ zip (cycle [False, True]) (randomDoubles r g)

randomDoubles :: (Double, Double) -> Int -> [Double]
randomDoubles r = randomRs r . mkStdGen
