{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module CairoImage (drawViewIO) where

import Foreign.Ptr
import Foreign.Marshal.Array
import Foreign.Storable
import Control.Monad.Primitive
import Data.Maybe
import Data.Array
import Data.OneOfThem
import Data.Word
import Data.Color
import Data.CairoContext
import Data.CairoImage

import Graphics.Cairo.Drawing.CairoT
import Graphics.Cairo.Drawing.Paths
import Graphics.Cairo.Drawing.Transformations
import Graphics.Cairo.Surfaces.ImageSurfaces
import Graphics.Cairo.Surfaces.PngSupport
import Graphics.Pango.Rendering.Cairo
import Graphics.Pango.Basic.LayoutObjects.PangoLayout
import Graphics.Pango.Basic.Fonts.PangoFontDescription

import Gpu.Vulkan.TypeEnum qualified as Vk.T
import Gpu.Vulkan.Object.Base qualified as Vk.ObjB

import Trial.Followbox.ViewType qualified as VT
import ConvertPixel

drawViewIO :: CairoSurfaceImageT s RealWorld ->
	CairoT r RealWorld -> VT.View -> IO CairoArgb32
drawViewIO sfc cr v = CairoArgb32 <$> drawView sfc cr v

newtype CairoArgb32 = CairoArgb32 Argb32 deriving Show
newtype PixelRgba d = PixelRgba (Rgba d) deriving Show

pixelArgb32ToPixelRgba :: PixelArgb32 -> PixelRgba d
pixelArgb32ToPixelRgba = PixelRgba . pixelArgb32ToRgba

pixelRgbaToPixelArgb32 :: RealFrac d => PixelRgba d -> PixelArgb32
pixelRgbaToPixelArgb32 (PixelRgba p) = rgbaToPixelArgb32 p

instance RealFrac d => Storable (PixelRgba d) where
	sizeOf _ = 4
	alignment _ = alignment @Word32 undefined
	peek p = do
		[r, g, b, a] <- peekArray 4 $ castPtr p
		pure . PixelRgba $ RgbaWord8 r g b a
	poke p (PixelRgba (RgbaWord8 r g b a)) =
		pokeArray (castPtr p) [r, g, b, a]

instance Vk.ObjB.IsImage CairoArgb32 where
	type ImagePixel CairoArgb32 = PixelRgba Double
	type ImageFormat CairoArgb32 = 'Vk.T.FormatR8g8b8a8Srgb
	imageRow (CairoArgb32 i) = fromIntegral . fst $ imageSize i
	imageWidth (CairoArgb32 i) = fromIntegral . fst $ imageSize i
	imageHeight (CairoArgb32 i) = fromIntegral . snd $ imageSize i
	imageDepth _ = 1
	imageBody (CairoArgb32 i) =
		(<$> [0 .. h - 1]) \y -> (<$> [0 .. w - 1]) \x ->
			pixelArgb32ToPixelRgba . fromJust $ pixelAt i x y
		where (w, h) = imageSize i
	imageMake (fromIntegral -> w) (fromIntegral -> h) _d pss =
		CairoArgb32 $ generateImage w h \x y ->
			pixelRgbaToPixelArgb32 $ (pss' ! y) ! x
		where pss' = listArray (0, h - 1) (listArray (0, w - 1) <$> pss)

drawView ::
	CairoSurfaceImageT s RealWorld -> CairoT r RealWorld -> VT.View ->
	IO Argb32
drawView sfc cr (VT.View vs) = do
	cairoSetSourceRgb cr . fromJust $ rgbDouble 0.7 0.7 0.7
	cairoPaint cr
	((drawLine cr >-- drawText cr >-- SingletonFun (drawImage cr)) `apply`)
		`mapM_` vs
	cairoImageSurfaceGetCairoImage sfc >>= \case
		CairoImageArgb32 i -> pure i
		_ -> error "never occur"

drawLine :: PrimMonad m => CairoT r (PrimState m) -> VT.Line -> m ()
drawLine cr (VT.Line' (VT.Color r g b) (realToFrac -> lw)
	(realToFrac -> x1, realToFrac -> y1)
	(realToFrac -> x2, realToFrac -> y2)) = do
	cairoSetSourceRgb cr $ RgbWord8 r g b
	cairoSetLineWidth cr lw
	cairoMoveTo cr x1 y1 >> cairoLineTo cr x2 y2
	cairoStroke cr

drawText :: CairoT r RealWorld -> VT.VText -> IO ()
drawText cr (VT.Text' (VT.Color r g b) fnm
	(realToFrac -> fsz) (realToFrac -> x, realToFrac -> y) txt) = do
	cairoSetSourceRgb cr $ RgbWord8 r g b
	pl <- pangoCairoCreateLayout cr
	pfd <- pangoFontDescriptionNew
	pangoFontDescriptionSet pfd $ Family fnm
	pangoFontDescriptionSet pfd $ AbsoluteSize fsz
	pangoLayoutSet pl . pangoFontDescriptionToNullable . Just
		=<< pangoFontDescriptionFreeze pfd
	pangoLayoutSet pl txt
	fpl <- pangoLayoutFreeze pl
	cairoMoveTo cr x y
	pangoCairoShowLayout cr fpl

drawImage :: PrimMonad m => CairoT r (PrimState m) -> VT.Image -> m ()
drawImage cr (VT.Image' (x, y) (VT.Png w h img)) = do
	sfc <- cairoSurfaceCreateFromPngByteString img
	w0 <- cairoImageSurfaceGetWidth sfc
	h0 <- cairoImageSurfaceGetHeight sfc
	cairoTranslate cr (realToFrac x) (realToFrac y)
	cairoScale cr
		(realToFrac w / fromIntegral w0)
		(realToFrac h / fromIntegral h0)
	cairoSetSourceSurface cr sfc 0 0
	cairoPaint cr
	cairoIdentityMatrix cr
