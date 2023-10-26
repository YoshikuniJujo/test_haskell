{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Juicy where

import Control.Monad.Primitive
import Data.CairoImage hiding (Image)
import Data.JuicyCairo
import Codec.Picture
import Graphics.Cairo.Surfaces.ImageSurfaces

cairoImageSurfaceCreateForJuicyImageRgba8 :: PrimMonad m =>
	Image PixelRGBA8 -> m (CairoSurfaceImageT s (PrimState m))
cairoImageSurfaceCreateForJuicyImageRgba8 =
	cairoImageSurfaceCreateForCairoImage . CairoImageArgb32 . juicyRGBA8ToCairoArgb32

cairoImageSurfaceGetJuicyImage :: PrimMonad m => CairoSurfaceImageT s (PrimState m) -> m DynamicImage
cairoImageSurfaceGetJuicyImage sfc = (<$> cairoImageSurfaceGetCairoImage sfc) \case
	CairoImageArgb32 a -> ImageRGBA8 $ cairoArgb32ToJuicyRGBA8 a
	_ -> error "yet"
