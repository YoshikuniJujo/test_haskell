{-# LANGUAGE GADTs, StandaloneDeriving #-}
{-# LANGUAGE DataKinds, KindSignatures #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.ImageData where

import Data.Angle
import Data.Color

data SurfaceType = Alpha | Rgba deriving Show

data Surface (t :: SurfaceType) = Surface {
	sfcWidth :: Integer, sfcHeight :: Integer, surfaceClips :: [Clip t] }
	deriving Show

data Clip (t :: SurfaceType) = Clip {
	clipBounds :: [[Path]], clipDraws :: [Draw t] }
	deriving Show

data Draw (t :: SurfaceType) = Draw {
	drawOperator :: Operator, drawSource :: Source t, drawMask :: Mask }
	deriving Show

data Operator
	= OperatorClear | OperatorSource | OperatorOver
	| OperatorIn | OperatorOut | OperatorAtop
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
	| MaskStroke LineWidth LineJoin [Path]
	| MaskFill [Path]
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

newtype LineWidth = LineWidth Double deriving Show

data LineJoin = LineJoinMiter Double | LineJoinRound | LineJoinBevel
	deriving Show

data Path
	= PathTransform Transform
	| Rectangle {
		rectX :: Double, rectY :: Double,
		rectWidth :: Double, rectHeight :: Double }
	| Arc {	arcCenterX :: Double, arcCenterY :: Double, arcRadius :: Double,
		arcAngleBegin :: Angle Double, srcAngleEnd :: Angle Double }
	| ArcNegative {
		arcCenterX :: Double, arcCenterY :: Double, arcRadius :: Double,
		arcAngleBegin :: Angle Double, srcAngleEnd :: Angle Double }
	deriving Show
