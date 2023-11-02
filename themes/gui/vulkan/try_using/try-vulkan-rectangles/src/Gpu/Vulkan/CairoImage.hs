{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.CairoImage (
	CairoArgb32, twoRectangles', twoRectanglesIO, twoRectanglesIO' ) where

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
import Graphics.Cairo.Surfaces.ImageSurfaces
import Gpu.Vulkan.TypeEnum qualified as Vk.T
import Gpu.Vulkan.Object.Base qualified as BObj

import Convert
import SampleImages
import Trial.Followbox.ViewType as VT

newtype CairoArgb32 = CairoArgb32 Argb32 deriving Show

twoRectangles' :: CairoArgb32
twoRectangles' = CairoArgb32 twoRectangles

twoRectanglesIO :: IO CairoArgb32
twoRectanglesIO = CairoArgb32 <$> twoRectanglesPrim

twoRectanglesIO' ::  CairoSurfaceImageT s RealWorld -> CairoT r RealWorld -> IO CairoArgb32
twoRectanglesIO' sfc cr = CairoArgb32 <$> twoRectanglesPrim' sfc cr

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

drawView :: PrimMonad m =>
	CairoSurfaceImageT s (PrimState m) -> CairoT r (PrimState m) -> View ->
	m Argb32
drawView sfc0 cr v = do
	cairoSetSourceRgb cr . fromJust $ rgbDouble 0.7 0.7 0.7
	cairoRectangle cr 0 0 256 256
	cairoFill cr

	cairoSetSourceRgb cr . fromJust $ rgbDouble 0.8 0.2 0.3
	cairoRectangle cr 50 50 110 110
	cairoFill cr

	cairoSetSourceRgb cr . fromJust $ rgbDouble 0.7 0.7 0.3
	cairoRectangle cr 100 130 100 70
	cairoFill cr

	cairoImageSurfaceGetCairoImage sfc0 >>= \case
		CairoImageArgb32 i -> pure i
		_ -> error "never occur"

drawLine :: PrimMonad m =>
	CairoSurfaceImageT s (PrimState m) -> CairoT r (PrimState m) -> Line ->
	m ()
drawLine sfc cr (Line' _ _ _ _) = do
	pure ()

drawText :: PrimMonad m =>
	CairoSurfaceImageT s (PrimState m) -> CairoT r (PrimState m) -> VText ->
	m ()
drawText sfc cr (Text' _ _ _ _ _) = do
	pure ()

drawImage :: PrimMonad m =>
	CairoSurfaceImageT s (PrimState m) -> CairoT r (PrimState m) -> VT.Image ->
	m ()
drawImage sfc cr (Image' _ _) = do
	pure ()
