{-# LANGUAGE ImportQualifiedPost #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control.Moffy.Samples.View where

import Foreign.C.Types
import Data.Text qualified as T
import Data.Color

data View
	= View [View1]
	| Stopped
	deriving Show

data View1
	= Box { leftUp :: Point, rightDown :: Point, color :: Rgb Double }
	| VLine (Rgb Double) LineWidth Point Point
	| VText (Rgb Double) FontName' FontSize' Position' T.Text
	| NotImplemented
	deriving Show

type LineWidth = Double
type Point = (Double, Double)

type FontName' = String
type FontSize' = Double
type Position' = (Double, Double)
