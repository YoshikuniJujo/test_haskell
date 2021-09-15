{-# LANGUAGE GADTs, StandaloneDeriving #-}
{-# LANGUAGE DataKinds, KindSignatures #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.ImageData where

import Data.Color

data SurfaceType = Alpha | Rgba deriving Show

data Surface (t :: SurfaceType) = Surface {
	sfcWidth :: Integer, sfcHeight :: Integer, surfaceDraws :: [Draw t] }
	deriving Show

data Draw (t :: SurfaceType) = Draw {
	drawTrans :: Transform, drawSource :: Source t, drawMask :: Mask }
	deriving Show

data Transform = Transform {
	transXx :: Double, transYx :: Double,
	transXy :: Double, transYy :: Double,
	transX0 :: Double, transY0 :: Double }
	deriving Show

data Source t = Source (Pattern 'Rgba) deriving Show

data Mask
	= MaskAlpha (Pattern 'Alpha)
	| MaskPaint Double
	| MaskStroke Shape
	| MaskFill Shape
--	| MaskGlyphs Glyphs
	deriving Show

data Pattern t
	= PatternSurface (Surface t)
	| PatternColor (SurfaceTypeColor t)
--	| PatternGradient foo bar
--	| PatternMesh foo bar
	deriving Show

data SurfaceTypeColor t where
	ColorAlpha :: Double -> SurfaceTypeColor 'Alpha
	ColorRgba :: Rgba -> SurfaceTypeColor 'Rgba

deriving instance Show (SurfaceTypeColor t)

data Shape = Shape {
	shapeLineWidth :: LineWidth,
	shapeLineJoin :: LineJoin,
	shapePaths :: Path }
	deriving Show

newtype LineWidth = LineWidth Double deriving Show

data LineJoin = LineJoinMiter Double | LineJoinRound | LineJoinBevel
	deriving Show

data Path
	= Rectangle {
		rectX :: Double, rectY :: Double,
		rectWidth :: Double, rectHeight :: Double }
	deriving Show
