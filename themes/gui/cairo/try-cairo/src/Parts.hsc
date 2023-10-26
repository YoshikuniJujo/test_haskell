{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Parts (
	testDraw, redSquare, checkPattern, readArgb32, writeArgb32,
	readArgb32Mut, writeArgb32Mut ) where

import Control.Monad
import Control.Monad.Primitive
import Data.Foldable
import Data.Maybe
import Data.Int
import Data.Color
import Data.CairoImage.Internal
import Data.JuicyCairo
import Codec.Picture
import Graphics.Cairo.Types
import Graphics.Cairo.Values
import Graphics.Cairo.Drawing.CairoT
import Graphics.Cairo.Drawing.Paths
import Graphics.Cairo.Surfaces.ImageSurfaces

import Data.CairoContext

#include <cairo.h>

testDraw :: FilePath -> #{type int} -> #{type int} -> (CairoT r RealWorld -> IO a) -> IO a
testDraw fp w h f = do
	putStrLn "*** TEST DRAW BEGIN ***"
	sfc0 <- cairoImageSurfaceCreate CairoFormatArgb32 w h
	cr <- cairoCreate sfc0
	f cr <* do
		cairoImageSurfaceGetCairoImage sfc0 >>= \case
			CairoImageArgb32 i -> writeArgb32 fp i
			_ -> error "image format error"
		putStrLn "*** TEST DRAW END ***"

writeArgb32 :: FilePath -> Argb32 -> IO ()
writeArgb32 fp = writePng fp . cairoArgb32ToJuicyRGBA8

writeArgb32Mut :: FilePath -> Argb32Mut RealWorld -> IO ()
writeArgb32Mut fp = (writePng fp =<<) . cairoArgb32MutToJuicyRGBA8

redSquare :: PrimMonad m => CairoT r (PrimState m) -> m ()
redSquare cr = do
	cairoSetSourceRgb cr . fromJust $ rgbDouble 1 0 0
	cairoRectangle cr 25 25 50 50
	cairoFill cr

checkPattern :: PrimMonad m => CairoT r (PrimState m) -> #{type int} -> #{type int} -> m ()
checkPattern cr w h = do
	cairoSetSourceRgb cr . fromJust $ rgbDouble 1 1 1
	for_ [0 .. (w - 1) `div` 25] \y -> for_ [0 .. (h - 1) `div` 25] \x ->
		when ((x + y) `mod` 2 == 0) $ box cr x y
	cairoFill cr
	cairoSetSourceRgb cr . fromJust $ rgbDouble 0.8 0.8 0.8
	for_ [0 .. (w - 1) `div` 25] \y -> for_ [0 .. (h - 1) `div` 25] \x ->
		when ((x + y) `mod` 2 == 1) $ box cr x y
	cairoFill cr

box :: PrimMonad m => CairoT r (PrimState m) -> #{type int} -> #{type int} -> m ()
box cr x y = cairoRectangle cr (fromIntegral x * 25) (fromIntegral y * 25) 25 25

readArgb32 :: FilePath -> IO Argb32
readArgb32 fp = readImage fp >>= \case
	Left emsg -> error emsg
	Right (ImageRGBA8 i) -> pure $ juicyRGBA8ToCairoArgb32 i
	_ -> error "image format error"

readArgb32Mut :: FilePath -> IO (Argb32Mut RealWorld)
readArgb32Mut fp = readImage fp >>= \case
	Left emsg -> error emsg
	Right (ImageRGBA8 i) -> juicyRGBA8ToCairoArgb32Mut i
	_ -> error "image format error"
