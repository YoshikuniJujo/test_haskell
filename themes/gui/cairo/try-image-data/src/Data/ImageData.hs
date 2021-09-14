{-# LANGUAGE GADTs, StandaloneDeriving #-}
{-# LANGUAGE DataKinds, KindSignatures #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.ImageData where

import Data.Color

data SurfaceType = Alpha | Rgba deriving Show

data Surface (t :: SurfaceType) = Surface {
	sfcWidth :: Integer, sfcHeight :: Integer,
	sfcTrans :: Transform, sfcSource :: Source t, sfcMask :: Mask }
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
--	| MaskStroke Path
--	| MaskFill Path
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
