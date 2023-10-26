{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

import Title

import Foreign.C.Types

import Control.Monad
import Control.Monad.ST
import Data.Foldable
import Data.Bool
import Data.Maybe
import Data.Color
import Data.CairoImage.Internal
import Codec.Picture hiding (pixelAt, generateImage)
import Graphics.Cairo.Drawing.CairoT
import Graphics.Cairo.Drawing.CairoPatternT.Basic
import Graphics.Cairo.Surfaces.ImageSurfaces

import Juicy

main :: IO ()
main = do
	putStrLn $ mkTitle "test a1"
	let	a1 = generateImage 33 33 (\x y -> PixelA1 $ circle x y) :: A1
	p <- cairoPatternCreateForSurface =<< cairoImageSurfaceCreateForCairoImage (CairoImageA1 a1)
	testPattern (0, 1, 1) "testA1.png" p
	a1m <- newImageMut 32 32 :: IO (A1Mut RealWorld)
	for_ [0 .. 32] \y -> for_ [0 .. 32] \x -> putPixel a1m x y . PixelA1 $ circle x y
	pm <- cairoPatternCreateForSurface =<< cairoImageSurfaceCreateForCairoImageMut (CairoImageMutA1 a1m)
	testPattern (1, 1, 0) "testA1Mut.png" pm

circle :: CInt -> CInt -> Bit
circle x_ y_ = bool O I . (< 256) $ sqrt (x ^ (2 :: Int) + y ^ (2 :: Int))
	where
	x = (fromIntegral x_ - 16) * 0x100 / 16 :: Double
	y = (fromIntegral y_ - 16) * 0x100 / 16 :: Double

type Color = (CDouble, CDouble, CDouble)

testPattern :: Color -> FilePath -> CairoPatternSurfaceT RealWorld -> IO ()
testPattern (r, g, b) fp p = do
	s <- cairoImageSurfaceCreate CairoFormatArgb32 33 33
	cr <- cairoCreate s
	cairoSetSourceRgb cr . fromJust $ rgbDouble 0 0 0
	cairoPaint cr
	cairoSetSourceRgb cr . fromJust $ rgbDouble r g b
	cairoMask cr p
	void $ writeDynamicPng fp =<< cairoImageSurfaceGetJuicyImage s
