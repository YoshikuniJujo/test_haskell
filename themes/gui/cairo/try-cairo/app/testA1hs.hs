{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

import Title

import Foreign.C.Types

import Control.Monad
import Control.Monad.ST
import Data.Maybe
import Data.Word
import Data.Color
import Data.CairoImage.Internal
import Data.JuicyCairo
import Codec.Picture hiding (pixelAt, generateImage)
import Graphics.Cairo.Drawing.CairoT
import Graphics.Cairo.Drawing.CairoPatternT.Basic
import Graphics.Cairo.Surfaces.ImageSurfaces
import Graphics.Cairo.Types
import Graphics.Cairo.Values

import Juicy

import System.Environment

main :: IO ()
main = do
	t : _ <- getArgs
	putStrLn $ mkTitle "test a1 hs"
	a1 <- readA1 (read t) "data/HaskellLogoGrayscaleWithAlpha.png"
	p <- cairoPatternCreateForSurface =<< cairoImageSurfaceCreateForCairoImage (CairoImageA1 a1)
	testPattern (0, 1, 0) "testA1hs.png" p

readA1 :: Word8 -> FilePath -> IO A1
readA1 t fp = readImage fp >>= \case
	Left emsg -> error emsg
	Right (ImageYA8 i) -> pure $ juicyYA8ToCairoA1 t i
	_ -> error "image format error"

type Color = (CDouble, CDouble, CDouble)

testPattern :: Color -> FilePath -> CairoPatternSurfaceT RealWorld -> IO ()
testPattern (r, g, b) fp p = do
	s <- cairoImageSurfaceCreate CairoFormatArgb32 128 128
	cr <- cairoCreate s
	cairoSetSourceRgb cr . fromJust $ rgbDouble 0 0 0
	cairoPaint cr
	cairoSetSourceRgb cr . fromJust $ rgbDouble r g b
	cairoMask cr p
	void $ writeDynamicPng fp =<< cairoImageSurfaceGetJuicyImage s
