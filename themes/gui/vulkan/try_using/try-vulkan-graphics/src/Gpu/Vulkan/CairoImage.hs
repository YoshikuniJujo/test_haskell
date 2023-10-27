{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.CairoImage (
	CairoArgb32, twoRectangles', twoRectanglesIO ) where

import Foreign.Ptr
import Foreign.Marshal.Array
import Foreign.Storable
import Data.Maybe
import Data.Array
import Data.Word
import Gpu.Vulkan.TypeEnum qualified as Vk.T
import Gpu.Vulkan.Object.Base qualified as BObj

import Convert
import SampleImages

newtype CairoArgb32 = CairoArgb32 Argb32 deriving Show

twoRectangles' :: CairoArgb32
twoRectangles' = CairoArgb32 twoRectangles

twoRectanglesIO :: IO CairoArgb32
twoRectanglesIO = CairoArgb32 <$> twoRectanglesPrim

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
