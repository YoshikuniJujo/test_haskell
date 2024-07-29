{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module CalcBall where

timeToHeight :: Double
timeToHeight = - 5 * 10 ** (- 4)

heightToTime :: Double -> Double
heightToTime h = sqrt $ h / (- timeToHeight)

bounce :: Double
bounce = 0.81

ballPos :: Double -> Double -> (Double, Double) -> (Double, Maybe Double)
ballPos t t0 (x, y) = (x, ballHeight t t0 y)

ballHeight :: Double -> Double -> Double -> Maybe Double
ballHeight t t0 h = getHeight (t - t0) h

height :: Double -> Double -> Double
height t h
	| t < heightToTime h = timeToHeight * t ^ (2 :: Int) + h
	| otherwise = timeToHeight * (t - (1 + sqrt bounce) * heightToTime h) ^ (2 :: Int) + bounce * h

getHeight :: Double -> Double -> Maybe Double
getHeight t h
	| t > 1000 && h < 1 = Nothing
	| h < 1 = Just 0
	| t < (1 + sqrt bounce) * heightToTime h = Just $ height t h
	| otherwise = getHeight (t - (1 + sqrt bounce) * heightToTime h) (bounce * h)
