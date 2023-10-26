{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

import Foreign.C.Types
import Control.Monad
import Control.Monad.Primitive
import Control.Monad.ST
import Data.Foldable
import Data.Maybe
import Data.Color
import Data.CairoImage.Internal
import Graphics.Cairo.Drawing.CairoT
import Graphics.Cairo.Drawing.Paths
import Graphics.Cairo.Surfaces.ImageSurfaces

import Codec.Picture hiding (pixelAt, generateImage)

import Juicy

import Data.CairoContext

main :: IO ()
main = do
	let	(_i, f, s) = runST red
	print (f, s)
	putStrLn ""
	print =<< redIo
	print $ runST green
	drawBlue
	drawYellow

makeRed :: PrimMonad m => m (CairoSurfaceImageT r (PrimState m), CairoT r (PrimState m))
makeRed = do
	s <- cairoImageSurfaceCreate CairoFormatArgb32 50 50
	cr <- cairoCreate $ CairoSurfaceTImage s
	cairoSetSourceRgb cr . fromJust $ rgbDouble 1 0 0
	pure (s, cr)

red :: forall s . ST s (DynamicImage, CairoFormatT, CInt) -- (Vector Word8, CairoFormatT, Int32)
red = do
	(s, cr) <- makeRed
	cairoPaint cr
	(,,) <$> cairoImageSurfaceGetJuicyImage s <*> cairoImageSurfaceGetFormat s <*> cairoImageSurfaceGetStride s

cairoImageSurfaceGetFormat s = cairoImageFormat <$> cairoImageSurfaceGetCairoImage s

cairoImageSurfaceGetStride s = cairoImageStride <$> cairoImageSurfaceGetCairoImage s

redIo :: IO (Either String Bool) -- DynamicImage -- (Vector Word8)
redIo = do
	(s, cr) <- makeRed
	cairoRectangle cr 0 0 25 25
	cairoFill cr
	cairoSetSourceRgb cr . fromJust $ rgbDouble 0 1 0
	cairoRectangle cr 25 0 25 25
	cairoFill cr
	cairoSetSourceRgb cr . fromJust $ rgbDouble 0.5 0.5 1
--	cairoSetSourceRgb cr 1 0 0
	cairoRectangle cr 0 25 25 25
	cairoFill cr
	cairoSetSourceRgb cr . fromJust $ rgbDouble 1 1 0
--	cairoSetSourceRgb cr 1 0 0
	cairoRectangle cr 25 25 25 25
	cairoFill cr
	writeDynamicPng "tmp.png" =<< cairoImageSurfaceGetJuicyImage s

green :: forall s . ST s (
	Either (Argb32, Maybe PixelArgb32, Maybe PixelArgb32, Maybe PixelArgb32) CairoImage,
	Maybe PixelArgb32 )
green = do
	s <- cairoImageSurfaceCreate CairoFormatArgb32 50 50
	cr <- cairoCreate s
	cairoSetSourceRgb cr . fromJust $ rgbDouble 0 1 0
	cairoPaint cr
	r1 <- (<$> cairoImageSurfaceGetCairoImage s) \case
		CairoImageArgb32 a -> Left (a, pixelAt a 0 0, pixelAt a 30 30, pixelAt a 49 49)
		i -> Right i
	cairoImageSurfaceGetCairoImageMut s >>= \case
		CairoImageMutArgb32 r2 -> do
			p2020 <- getPixel r2 20 20
			pure (r1, p2020)
		i -> error $ "green: " ++ show i

blue :: Argb32
blue = generateImage 200 200 \_ _ -> PixelArgb32Word32 0xff0000ff

blueSurface :: ST s (CairoSurfaceImageT r s)
blueSurface = cairoImageSurfaceCreateForCairoImage $ CairoImageArgb32 blue

drawBlue :: IO ()
drawBlue = do
	void . writeDynamicPng "tmp2.png" $ runST (cairoImageSurfaceGetJuicyImage =<< blueSurface)
	pure ()

yellow :: ST s (Argb32Mut s)
yellow = do
	i <- newImageMut 200 200
	for_ [0 .. 199] \h -> for_ [0 .. 199] \w -> putPixel i w h $ PixelArgb32Word32 0xffffff00
	pure i

yellowSurface :: ST s (CairoSurfaceImageT r s)
yellowSurface = cairoImageSurfaceCreateForCairoImageMut . CairoImageMutArgb32 =<< yellow

drawYellow :: IO ()
drawYellow = do
	void . writeDynamicPng "tmp3.png" $ runST (cairoImageSurfaceGetJuicyImage =<< yellowSurface)
	pure ()
