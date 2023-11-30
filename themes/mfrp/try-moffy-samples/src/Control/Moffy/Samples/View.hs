module Control.Moffy.Samples.View where

import Foreign.C.Types
import Data.Color

data View
	= View [View1]
	| Stopped
	deriving Show

data View1
	= Box { leftUp :: Point, rightDown :: Point, color :: Rgb Double }
	| VLine (Rgb Double) LineWidth Point Point
	| NotImplemented
	deriving Show

type LineWidth = Double
type Point = (Double, Double)
