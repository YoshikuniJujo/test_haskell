module Control.Moffy.Samples.View where

data View
	= Rect { leftUp :: Point, rightDown :: Point }
	| Position Point
	| Stopped
	deriving Show

type Point = (Double, Double)
