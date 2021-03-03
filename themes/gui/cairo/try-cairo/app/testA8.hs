{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

import Title

import Control.Monad
import Control.Monad.ST
import Data.Foldable
import Data.Word
import Data.Int
import Data.CairoImage.Internal
import Graphics.Cairo.Drawing.CairoT
import Graphics.Cairo.Drawing.CairoPatternT
import Graphics.Cairo.Surfaces.ImageSurfaces
import Graphics.Cairo.Surfaces.ImageSurfaces.Juicy
import Graphics.Cairo.Types
import Graphics.Cairo.Values

import Codec.Picture hiding (pixelAt, generateImage)

main :: IO ()
main = do
	putStrLn $ mkTitle "test a8"
	let	a8 = generateImage 33 33 (\x y -> PixelA8 $ circle x y) :: A8
	p <- cairoPatternCreateForSurface =<< cairoImageSurfaceCreateForCairoImage (CairoImageA8 a8)
	testPattern (0, 1, 1) "testA8.png" p
	a8m <- newImageMut 32 32 :: IO (A8Mut RealWorld)
	for_ [0 .. 32] \y -> for_ [0 .. 32] \x -> putPixel a8m x y . PixelA8 $ circle x y
	pm <- cairoPatternCreateForSurface =<< cairoImageSurfaceCreateForCairoImageMut (CairoImageMutA8 a8m)
	testPattern (1, 1, 0) "testA8Mut.png" pm

circle :: Int32 -> Int32 -> Word8
circle x_ y_ = round $ sqrt (x ^ (2 :: Int) + y ^ (2 :: Int))
	where
	x = (fromIntegral x_ - 16) * 0x100 / 16 :: Double
	y = (fromIntegral y_ - 16) * 0x100 / 16 :: Double

type Color = (Double, Double, Double)

testPattern :: Color -> FilePath -> CairoPatternT RealWorld -> IO ()
testPattern (r, g, b) fp p = do
	s <- cairoImageSurfaceCreate cairoFormatArgb32 33 33
	cr <- cairoCreate s
	cairoSetSourceRgb cr 0 0 0
	cairoPaint cr
	cairoSetSourceRgb cr r g b
	cairoMask cr p
	void $ writeDynamicPng fp =<< cairoImageSurfaceGetJuicyImage s
