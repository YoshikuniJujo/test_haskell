{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

import Title

import Foreign.C.Types

import Control.Monad
import Control.Monad.ST
import Data.Maybe
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

main :: IO ()
main = do
	putStrLn $ mkTitle "test a8 hs RGBA"
	a8 <- readA8 "data/HaskellLogo.png"
	p <- cairoPatternCreateForSurface =<< cairoImageSurfaceCreateForCairoImage (CairoImageA8 a8)
	testPattern (0, 1, 0) "testA8hsRGBA.png" p

readA8 :: FilePath -> IO A8
readA8 fp = readImage fp >>= \case
	Left emsg -> error emsg
	Right (ImageRGBA8 i) -> pure $ juicyRGBA8ToCairoA8 i
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
