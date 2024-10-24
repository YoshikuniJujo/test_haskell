{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module DrawView (drawView, CairoArgb32) where

import Foreign.C.Types
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

import Data.CairoContext

import Data.Maybe.ToolsYj

---------------------------------------------------------------------------
--
-- * DRAW VIEW
-- * CAIRO ARGB 32
--
---------------------------------------------------------------------------

-- DRAW VIEW

drawView :: CairoSurfaceImageT s RealWorld ->
	CairoT r RealWorld -> View -> IO CairoArgb32
drawView sfc cr (View vs) = CairoArgb32 <$> do
	drRect cr (fromJust $ rgbDouble 0.7 0.7 0.7) 0 0 1024 1024
	drRect cr (fromJust $ rgbDouble 0.8 0.2 0.3) 50 50 110 110
	drRect cr (fromJust $ rgbDouble 0.7 0.7 0.3) 100 130 100 70
	((drLn cr >-- drTxt cr >-- SingletonFun (drImg cr)) `apply`) `mapM_` vs
	cairoImageSurfaceGetCairoImage sfc >>= \case
		CairoImageArgb32 i -> pure i
		_ -> error "not Cairo Image ARGB 32"

drRect :: PrimMonad m => CairoT r (PrimState m) ->
	Rgb CDouble -> CDouble -> CDouble -> CDouble -> CDouble -> m ()
drRect cr clr l t w h =
	cairoSetSourceRgb cr clr >> cairoRectangle cr l t w h >> cairoFill cr

drLn :: PrimMonad m => CairoT r (PrimState m) -> Line -> m ()
drLn cr (Line' (Color r g b) (realToFrac -> lw)
	(realToFrac -> x1, realToFrac -> y1)
	(realToFrac -> x2, realToFrac -> y2)) = do
	cairoSetSourceRgb cr $ RgbWord8 r g b
	cairoSetLineWidth cr lw
	cairoMoveTo cr x1 y1 >> cairoLineTo cr x2 y2
	cairoStroke cr

drTxt :: CairoT r RealWorld -> VText -> IO ()
drTxt cr (Text' (Color r g b) fnm (realToFrac -> fsz)
	(realToFrac -> x, realToFrac -> y) txt) = do
	cairoSetSourceRgb cr $ RgbWord8 r g b
	(l, fd) <- (,) <$> pangoCairoCreateLayout cr <*> pangoFontDescriptionNew
	pangoFontDescriptionSet fd $ Family fnm
	pangoFontDescriptionSet fd $ AbsoluteSize fsz
	pangoLayoutSet l . pangoFontDescriptionToNullable
		. Just =<< pangoFontDescriptionFreeze fd
	pangoLayoutSet l txt
	fl <- pangoLayoutFreeze l
	cairoMoveTo cr x y
	pangoCairoShowLayout cr fl

drImg :: PrimMonad m => CairoT r (PrimState m) -> VT.Image -> m ()
drImg cr (Image' (x, y) (Png w h img)) = do
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

-- CAIRO ARGB 32

newtype CairoArgb32 = CairoArgb32 Argb32 deriving Show

instance BObj.IsImage CairoArgb32 where
	type ImagePixel CairoArgb32 = PixelRgba Double
	type ImageFormat CairoArgb32 = 'Vk.T.FormatR8g8b8a8Srgb
	imageRow (CairoArgb32 i) = fromIntegral . fst $ imageSize i
	imageWidth (CairoArgb32 i) = fromIntegral . fst $ imageSize i
	imageHeight (CairoArgb32 i) = fromIntegral . snd $ imageSize i
	imageDepth _ = 1
	imageBody (CairoArgb32 i@(imageSize -> (w, h))) =
		(<$> [0 .. w - 1]) \y -> (<$> [0 .. h - 1]) \x ->
			pixelArgb32ToRgba . fromJust $ pixelAt i x y
	imageMake (fromIntegral -> w) (fromIntegral -> h) _d pss =
		CairoArgb32 $ generateImage w h \x y ->
			pixelRgbaToArgb32 $ (pss' ! y) ! x
		where pss' = listArray (0, h - 1) (listArray (0, w - 1) <$> pss)

newtype PixelRgba d = PixelRgba (Rgba d) deriving Show

instance RealFrac d => Storable (PixelRgba d) where
	sizeOf _ = 4; alignment _ = alignment @Word32 undefined
	peek (castPtr -> p) = do
		[r, g, b, a] <- peekArray 4 p
		pure . PixelRgba $ RgbaWord8 r g b a
	poke (castPtr -> p) (PixelRgba (RgbaWord8 r g b a)) =
		pokeArray p [r, g, b, a]

pixelArgb32ToRgba :: PixelArgb32 -> PixelRgba d
pixelArgb32ToRgba (PixelArgb32Premultiplied a r g b) = PixelRgba
	. forceJust' (
		"pixelArgb32ToRgba: (a, r, g, b) = (" ++
		show a ++ ", " ++ show r ++ ", " ++
		show g ++ ", " ++ show b ++ ")" )
	$ rgbaPremultipliedWord8 r g b a

pixelRgbaToArgb32 :: RealFrac d => PixelRgba d -> PixelArgb32
pixelRgbaToArgb32 (PixelRgba (RgbaPremultipliedWord8 a r g b)) =
	fromJust $ pixelArgb32Premultiplied a r g b
