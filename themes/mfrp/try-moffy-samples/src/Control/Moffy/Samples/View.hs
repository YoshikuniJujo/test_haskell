module Control.Moffy.Samples.View where

import Data.Color

data View
	= View [View1]
	| Stopped
	deriving Show

data View1 = Box { leftUp :: Point, rightDown :: Point, color :: Rgb Double }
	deriving Show

type Point = (Double, Double)
