{-# LANGUAGE TypeFamilies, GADTs, StandaloneDeriving #-}
{-# LANGUAGE DataKinds, KindSignatures #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.ImageData where

import Data.Angle
import Data.Color
import Data.CairoImage

import Data.ImageData.Text

data SurfaceType = Alpha | Rgba deriving Show

data Surface (t :: SurfaceType) =
	Surface { surfaceBase :: SurfaceBase t, surfaceClips :: DrawScript t }
	deriving Show

data SurfaceBase (t :: SurfaceType) where
	SurfaceBaseBlank ::
		{ surfaceBaseWidth :: Integer, surfaceBaseHeight :: Integer } -> SurfaceBase t
	SurfaceBaseA8 :: A8 -> SurfaceBase 'Alpha
	SurfaceBaseArgb32 :: Argb32 -> SurfaceBase 'Rgba

deriving instance Show (SurfaceBase t)

type DrawScript t = [Clip t]

data Clip (t :: SurfaceType) = Clip {
	clipBounds :: [[Path]], clipDraws :: [Draw t] }
	deriving Show

data Draw (t :: SurfaceType) = Draw {
	drawOperator :: Operator, drawSource :: Source t, drawMask :: Mask }
	deriving Show

data Operator
	= OperatorClear | OperatorSource | OperatorOver
	| OperatorIn | OperatorOut | OperatorAtop
	| OperatorDest | OperatorDestOver | OperatorDestIn | OperatorDestOut
	| OperatorDestAtop | OperatorXor | OperatorAdd | OperatorSaturate
	| OperatorMultiply | OperatorScreen | OperatorOverlay
	| OperatorDarken | OperatorLighten
	| OperatorColorDodge | OperatorColorBurn
	| OperatorHardLight | OperatorSoftLight
	| OperatorDifference | OperatorExclusion
	| OperatorHslHue | OperatorHslSaturation
	| OperatorHslColor | OperatorHslLuminosity
	deriving (Show, Enum)

data Transform = Transform {
	transXx :: Double, transYx :: Double,
	transXy :: Double, transYy :: Double,
	transX0 :: Double, transY0 :: Double }
	deriving Show

data Source t = Source (Pattern t) deriving Show

data Mask
	= MaskAlpha (Pattern 'Alpha)
	| MaskPaint Double
	| MaskStroke LineWidth LineDash LineCap LineJoin [Path]
	| MaskFill [Path]
	| MaskTextLayout Transform Layout
	deriving Show

data Pattern t
	= PatternSolid (SurfaceTypeColor t)
	| PatternNonSolid {
		patternFilter :: PatternFilter,
		patternExtend :: PatternExtend,
		patternMatrix :: Transform,
		patternBody :: PatternNonSolid t }
--	| PatternGradient Transform foo bar
--	| PatternMesh Transform foo bar
	deriving Show

data PatternFilter
	= PatternFilterFast | PatternFilterGood | PatternFilterBest
	| PatternFilterNearest | PatternFilterBilinear
	deriving Show

data PatternExtend
	= PatternExtendNone | PatternExtendRepeat
	| PatternExtendReflect | PatternExtendPad
	deriving Show

data SurfaceTypeColor t where
	ColorAlpha :: Double -> SurfaceTypeColor 'Alpha
	ColorRgba :: Rgba Double -> SurfaceTypeColor 'Rgba

data PatternNonSolid t
	= PatternSurface (Surface t)
	deriving Show

deriving instance Show (SurfaceTypeColor t)

newtype LineWidth = LineWidth Double deriving Show

data LineDash = LineDash {
	lineDashDashes :: [Double],
	lineDashOffset :: Double } deriving Show

data LineCap = LineCapButt | LineCapRound | LineCapSquare deriving Show

data LineJoin = LineJoinMiter Double | LineJoinRound | LineJoinBevel
	deriving Show

data Path
	= PathTransform Transform
	| MoveTo { moveToX :: Double, moveToY :: Double }
	| LineTo { lineToX :: Double, lineToY :: Double }
	| Rectangle {
		rectX :: Double, rectY :: Double,
		rectWidth :: Double, rectHeight :: Double }
	| Arc {	arcCenterX :: Double, arcCenterY :: Double, arcRadius :: Double,
		arcAngleBegin :: Angle Double, srcAngleEnd :: Angle Double }
	| ArcNegative {
		arcCenterX :: Double, arcCenterY :: Double, arcRadius :: Double,
		arcAngleBegin :: Angle Double, srcAngleEnd :: Angle Double }
	deriving Show
