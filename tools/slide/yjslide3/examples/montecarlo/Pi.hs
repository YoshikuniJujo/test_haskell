module Pi where

import System.Random

getPi :: Int -> Int -> Double
getPi t g = fromIntegral (length pins) / fromIntegral t * 4
	where
	pins = filter inCircle $ take t $ randomXY g

inCircle :: (Double, Double) -> Bool
inCircle (x, y) = x ^ 2 + y ^ 2 <= 1

randomXY :: Int -> [(Double, Double)]
randomXY g = zip (randomX g) (randomY g)

randomX :: Int -> [Double]
randomX g = map snd $ filter fst $ zip (cycle [True, False]) (randomDoubles g)

randomY :: Int -> [Double]
randomY g = map snd $ filter fst $ zip (cycle [False, True]) (randomDoubles g)

randomDoubles :: Int -> [Double]
randomDoubles = randomRs (-1, 1) . mkStdGen
