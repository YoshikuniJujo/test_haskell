{-# LANGUAGE GADTs, StandaloneDeriving #-}
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

data SurfaceBase (t :: SurfaceType)
	= SurfaceBaseBlank {
		surfaceBaseWidth :: Integer, surfaceBaseHeight :: Integer }
	deriving Show

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
	| MaskStroke LineWidth LineJoin [Path]
	| MaskFill [Path]
	| MaskTextLayout Transform Layout
	deriving Show

data Pattern t
	= PatternSurface Transform (Surface t)
	| PatternColor (SurfaceTypeColor t)
--	| PatternGradient Transform foo bar
--	| PatternMesh Transform foo bar
	deriving Show

data SurfaceTypeColor t where
	ColorAlpha :: Double -> SurfaceTypeColor 'Alpha
	ColorRgba :: Rgba Double -> SurfaceTypeColor 'Rgba

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
