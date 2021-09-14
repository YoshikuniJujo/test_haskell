{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Trial.TryCairo where

import Control.Monad.ST
import Data.ImageData
import Data.CairoContext
import Graphics.Cairo.Drawing.CairoT
import Graphics.Cairo.Drawing.Transformations
import Graphics.Cairo.Utilities.CairoMatrixT
import Graphics.Cairo.Surfaces.ImageSurfaces
import Graphics.Cairo.Values

drawSurface :: Surface 'Rgba -> IO (CairoSurfaceImageT s RealWorld)
drawSurface sfc@Surface {
	sfcWidth = (fromInteger -> w), sfcHeight = (fromInteger -> h) } = do
	sr <- cairoImageSurfaceCreate cairoFormatArgb32 w h
	cr <- cairoCreate sr
	cairoDrawSurface cr sfc
	pure sr

cairoDrawSurface :: CairoTIO s -> Surface 'Rgba -> IO ()
cairoDrawSurface cr Surface
	{ sfcTrans = tr, sfcSource = src, sfcMask = msk } = do
	cairoTransform cr =<< transToCairoMatrixT tr
	cairoDrawSource cr src
	cairoDrawMask cr msk

transToCairoMatrixT :: Transform -> IO (CairoMatrixT RealWorld)
transToCairoMatrixT (Transform xx_ yx_ xy_ yy_ x0_ y0_) =
	cairoMatrixNew xx yx xy yy x0 y0
	where
	[xx, yx, xy, yy, x0, y0] = realToFrac <$> [xx_, xy_, yx_, yy_, x0_, y0_]

cairoDrawSource :: CairoTIO s -> Source 'Rgba -> IO ()
cairoDrawSource cr (Source ptn) = case ptn of
	PatternSurface sfc -> error "yet"
	PatternColor (ColorRgba clr) -> cairoSetSourceRgba cr clr

cairoDrawMask :: CairoTIO s -> Mask -> IO ()
cairoDrawMask cr = \case
	MaskAlpha alp -> error "yet"
	MaskPaint alp -> cairoPaintWithAlpha cr $ realToFrac alp
