module CloseShape where

data Shape
	= Rectangle (Double, Double) Double Double
	| Circle (Double, Double) Double

area :: Shape -> Double
area (Rectangle _ w h) = w * h
area (Circle _ r) = r * r * pi

areas :: [Shape] -> [Double]
areas = map area
