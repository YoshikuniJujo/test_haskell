{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.CairoImage (
	CairoArgb32, twoRectangles', twoRectanglesIO, twoRectanglesIO',
	drawViewIO ) where

import Foreign.Ptr
import Foreign.Marshal.Array
import Foreign.Storable
import Control.Monad.Primitive
import Data.Maybe
import Data.Array
import Data.Word
import Data.Color
import Data.CairoImage
import Graphics.Cairo.Drawing.CairoT
import Graphics.Cairo.Drawing.Paths
import Graphics.Cairo.Drawing.Transformations
import Graphics.Cairo.Surfaces.ImageSurfaces
import Gpu.Vulkan.TypeEnum qualified as Vk.T
import Gpu.Vulkan.Object.Base qualified as BObj

import Trial.Followbox.ViewType as VT

import Data.OneOfThem

import Graphics.Pango.Basic.Fonts.PangoFontDescription
import Graphics.Pango.Basic.LayoutObjects.PangoLayout
import Graphics.Pango.Rendering.Cairo

import Graphics.Cairo.Surfaces.PngSupport

import Control.Monad.ST
import Data.CairoContext

import Graphics.Cairo.Surfaces.CairoSurfaceT
import Data.CairoImage.Internal

newtype CairoArgb32 = CairoArgb32 Argb32 deriving Show

twoRectangles' :: CairoArgb32
twoRectangles' = CairoArgb32 twoRectangles

twoRectanglesIO :: IO CairoArgb32
twoRectanglesIO = CairoArgb32 <$> twoRectanglesPrim

twoRectanglesIO' ::  CairoSurfaceImageT s RealWorld -> CairoT r RealWorld -> IO CairoArgb32
twoRectanglesIO' sfc cr = CairoArgb32 <$> twoRectanglesPrim' sfc cr

drawViewIO :: CairoSurfaceImageT s RealWorld -> CairoT r RealWorld -> View -> IO CairoArgb32
drawViewIO sfc cr v = CairoArgb32 <$> drawView sfc cr v

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

{-
instance BObj.IsImage CairoArgb32 where
	type ImagePixel CairoArgb32 = PixelRgba Double
	type ImageFormat CairoArgb32 = 'Vk.T.FormatR8g8b8a8Srgb
	imageRow (CairoArgb32 img) = 16
	imageWidth (CairoArgb32 img) = 16
	imageHeight (CairoArgb32 img) = 16
	imageDepth _ = 1
	imageBody (CairoArgb32 img) =
		(<$> [0 .. w - 1]) \y -> (<$> [0 .. h - 1]) \x -> PixelRgba $ RgbaWord8 0xff 0x00 0x00 0xff
		where (w, h) = (16, 16)
		{-
	imageMake (fromIntegral -> w) (fromIntegral -> h) _d pss =
		CairoArgb32 $ generateImage w h \x y ->
			pixelRgbaToPixelArgb32 $ (pss' ! y) ! x
		where pss' = listArray (0, h - 1) (listArray (0, w - 1) <$> pss)
		-}
		-}

instance BObj.IsImage CairoArgb32 where
	type ImagePixel CairoArgb32 = PixelRgba Double
	type ImageFormat CairoArgb32 = 'Vk.T.FormatR8g8b8a8Srgb
	imageRow (CairoArgb32 img) = fromIntegral . fst $ imageSize img
	imageWidth (CairoArgb32 img) = fromIntegral . fst $ imageSize img
	imageHeight (CairoArgb32 img) = fromIntegral . snd $ imageSize img
	imageDepth _ = 1
	imageBody (CairoArgb32 img) =
		(<$> [0 .. w - 1]) \y -> (<$> [0 .. h - 1]) \x ->
			pixelArgb32ToPixelRgba . fromJust $ pixelAt img x y
		where (w, h) = imageSize img
	imageMake (fromIntegral -> w) (fromIntegral -> h) _d pss =
		CairoArgb32 $ generateImage w h \x y ->
			pixelRgbaToPixelArgb32 $ (pss' ! y) ! x
		where pss' = listArray (0, h - 1) (listArray (0, w - 1) <$> pss)

drawView ::
	CairoSurfaceImageT s RealWorld -> CairoT r RealWorld -> View ->
	IO Argb32
drawView sfc0 cr (View vs) = do
	cairoSetSourceRgb cr . fromJust $ rgbDouble 0.7 0.7 0.7
	cairoRectangle cr 0 0 1024 1024
	cairoFill cr

	cairoSetSourceRgb cr . fromJust $ rgbDouble 0.8 0.2 0.3
	cairoRectangle cr 50 50 110 110
	cairoFill cr

	cairoSetSourceRgb cr . fromJust $ rgbDouble 0.7 0.7 0.3
	cairoRectangle cr 100 130 100 70
	cairoFill cr

	((drawLine cr >-- drawText cr >-- SingletonFun (drawImage cr)) `apply`)
		`mapM_` vs

	cairoImageSurfaceGetCairoImage sfc0 >>= \case
		CairoImageArgb32 i -> pure i
		_ -> error "never occur"

drawLine :: PrimMonad m => CairoT r (PrimState m) -> Line -> m ()
drawLine cr (Line' (Color r g b) (realToFrac -> lw)
	(realToFrac -> x1, realToFrac -> y1)
	(realToFrac -> x2, realToFrac -> y2)) = do
	cairoSetSourceRgb cr $ RgbWord8 r g b
	cairoSetLineWidth cr lw
	cairoMoveTo cr x1 y1
	cairoLineTo cr x2 y2
	cairoStroke cr

drawText :: CairoT r RealWorld -> VText -> IO ()
drawText cr (Text' (Color r g b) fnm (realToFrac -> fsz) (realToFrac -> x, realToFrac -> y) txt) = do
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
drawImage cr (Image' (x, y) (Png w h img)) = do

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

pixelArgb32ToRgba :: PixelArgb32 -> Rgba d
pixelArgb32ToRgba (PixelArgb32Premultiplied a r g b) =
	fromJustWithErrorMsg (
			"pixelArgb32ToRgba: (a, r, g, b) = (" ++
			show a ++ ", " ++ show r ++ ", " ++
			show g ++ ", " ++ show b ++ ")" )
		$ rgbaPremultipliedWord8 r g b a

rgbaToPixelArgb32 :: RealFrac d => Rgba d -> PixelArgb32
rgbaToPixelArgb32 (RgbaPremultipliedWord8 a r g b) =
	fromJust $ pixelArgb32Premultiplied a r g b

fromJustWithErrorMsg :: String -> Maybe a -> a
fromJustWithErrorMsg msg = \case
	Nothing -> error msg
	Just x -> x

twoRectangles :: Argb32
twoRectangles = runST twoRectanglesPrim

twoRectanglesPrim :: PrimMonad m => m Argb32
twoRectanglesPrim = do
	sfc0 <- cairoImageSurfaceCreate CairoFormatArgb32 256 256
	cr <- cairoCreate sfc0

	twoRectanglesPrim' sfc0 cr

twoRectanglesPrim' :: PrimMonad m =>
	CairoSurfaceImageT s (PrimState m) -> CairoT r (PrimState m) -> m Argb32
twoRectanglesPrim' sfc0 cr = do
	cairoSetSourceRgb cr . fromJust $ rgbDouble 0.7 0.7 0.7
	cairoRectangle cr 0 0 256 256
	cairoFill cr

	cairoSetSourceRgb cr . fromJust $ rgbDouble 0.8 0.2 0.3
	cairoRectangle cr 50 50 110 110
	cairoFill cr

	cairoSetSourceRgb cr . fromJust $ rgbDouble 0.7 0.7 0.3
	cairoRectangle cr 100 130 100 70
	cairoFill cr

	cairoSurfaceFlush sfc0

	cairoImageSurfaceGetCairoImage sfc0 >>= \case
		CairoImageArgb32 i -> pure i
		_ -> error "never occur"
