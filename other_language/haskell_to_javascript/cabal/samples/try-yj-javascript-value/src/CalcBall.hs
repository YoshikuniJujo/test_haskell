{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module CalcBall (ballPos) where

import Control.Arrow

timeToHeight :: Double
timeToHeight = - 5 * 10 ** (- 4)

bounce :: Double
bounce = 0.81

ballPos :: Double -> Double -> (Double, Double) -> (Double, Maybe Double)
ballPos t = second . getHeight . (t -)

getHeight :: Double -> Double -> Maybe Double
getHeight t h
	| t > 1000 && h < 1 = Nothing | h < 1 = Just 0
	| t < heightToTime2 h = Just $ height t h
	| otherwise = getHeight (t - heightToTime2 h) (bounce * h)

height :: Double -> Double -> Double
height t h
	| t < heightToTime h = timeToHeight * t ^ (2 :: Int) + h
	| otherwise =
		timeToHeight * (t - heightToTime2 h) ^ (2 :: Int) + bounce * h

heightToTime :: Double -> Double
heightToTime = sqrt . (/ (- timeToHeight))

heightToTime2 :: Double -> Double
heightToTime2 = ((1 + sqrt bounce) *) . heightToTime
