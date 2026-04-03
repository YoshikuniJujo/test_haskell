{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module SampleImages (simpleGlaph) where

import Foreign.C
import Control.Monad.Primitive
import Control.Monad.ST
import Data.Int
import Data.CairoImage.Internal
import Data.CairoContext
import Graphics.Cairo.Surfaces.ImageSurfaces
import Graphics.Cairo.Drawing.CairoT
import Graphics.Cairo.Drawing.Paths

simpleGlaph :: (Real n, Real n') => Int32 -> Int32 -> n -> n -> n' -> n' -> [n] -> Argb32
simpleGlaph w h
	(realToFrac -> x0) (realToFrac -> y0) (realToFrac -> dx) (realToFrac -> dy) ((realToFrac <$>) -> vs) =
	case runST put of
		CairoImageArgb32 i -> i
		_ -> error "never occur"
	where
	put :: ST s CairoImage
	put = do
		sfc0 <- cairoImageSurfaceCreate CairoFormatArgb32 w h
		cr <- cairoCreate sfc0
		putSimpleGlaph cr x0 dx (((+ y0) . (* dy)) <$> vs)
		cairoImageSurfaceGetCairoImage sfc0

putSimpleGlaph :: PrimMonad m =>
	CairoT r (PrimState m) -> CDouble -> CDouble -> [CDouble] -> m ()
putSimpleGlaph _ _ _ [] = pure ()
putSimpleGlaph cr x0 dx (v0 : va) = do
	cairoMoveTo cr x0 v0
	go (x0 + dx) va
	where
	go _ [] = cairoStroke cr
	go x (v : vs) = do
		cairoLineTo cr x v
		go (x + dx) vs
