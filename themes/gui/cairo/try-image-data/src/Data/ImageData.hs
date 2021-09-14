{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.ImageData where

import Data.Color

data Surface =
	Surface { sfcTrans :: Transform, sfcSource :: Source, sfcMask :: Mask }
	deriving Show

data Transform = Transform {
	transXx :: Double, transYx :: Double,
	transXy :: Double, transYy :: Double,
	transX0 :: Double, transY0 :: Double }
	deriving Show

data Source = Source Pattern deriving Show

data Mask
	= MaskAlpha Alpha
	| MaskPaint Double
--	| MaskStroke Path
--	| MaskFill Path
--	| MaskGlyphs Glyphs
	deriving Show

data Alpha = Alpha Pattern deriving Show

data Pattern
	= PatternSurface Surface
	| PatternColor Rgba
--	| PatternGradient foo bar
--	| PatternMesh foo bar
	deriving Show
