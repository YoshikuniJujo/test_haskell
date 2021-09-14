{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Trial.TryCairo where

import Control.Monad.ST
import Data.ImageData
import Data.CairoContext
import Graphics.Cairo.Drawing.CairoT
import Graphics.Cairo.Drawing.Transformations
import Graphics.Cairo.Utilities.CairoMatrixT

cairoDrawSurface :: CairoTIO s -> Surface -> IO ()
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

cairoDrawSource :: CairoTIO s -> Source -> IO ()
cairoDrawSource cr (Source ptn) = case ptn of
	PatternSurface sfc -> error "yet"
	PatternColor clr -> cairoSetSourceRgba cr clr

cairoDrawMask :: CairoTIO s -> Mask -> IO ()
cairoDrawMask cr = \case
	MaskAlpha alp -> error "yet"
	MaskPaint alp -> cairoPaintWithAlpha cr $ realToFrac alp
