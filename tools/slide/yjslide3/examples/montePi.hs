import System.Random (StdGen, randomRs, mkStdGen)

montePi :: Int -> StdGen -> Double
montePi n sg = 4 * fromIntegral (length inps) / fromIntegral n
	where
	inps = filter (uncurry inCircle) $ take n $ points sg

inCircle :: Double -> Double -> Bool
inCircle x y = x ^ 2 + y ^ 2 < 1

points :: StdGen -> [(Double, Double)]
points sg = map snd $ filter fst $ zip (cycle [True, False]) $ zip ds $ tail ds
	where
	ds = randomRs (-1, 1) sg
