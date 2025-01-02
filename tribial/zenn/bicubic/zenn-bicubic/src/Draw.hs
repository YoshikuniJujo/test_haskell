{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Draw (
	withPng, withPng',
	fill, strokeAndFill, circle, rectangle,
	grid, Margin(..), coordinateX, coordinateY
	) where

import Foreign.C.Types
import Control.Monad.Primitive
import Data.Foldable
import Data.Maybe
import Data.Int
import Data.Color
import Data.CairoImage.Internal
import Data.JuicyCairo
import Data.CairoContext
import Codec.Picture
import Graphics.Cairo.Drawing.CairoT
import Graphics.Cairo.Drawing.CairoT.Setting
import Graphics.Cairo.Surfaces.ImageSurfaces
import Graphics.Cairo.Drawing.Paths

withPng :: FilePath -> Int32 -> Int32 -> (CairoT r RealWorld -> IO a) -> IO ()
withPng fp w h f = do
	s <- cairoImageSurfaceCreate CairoFormatArgb32 w h
	cr <- cairoCreate s
	_ <- f cr
	cairoImageSurfaceGetCairoImage s >>= \case
		CairoImageArgb32 a -> writePng fp $ cairoArgb32ToJuicyRGBA8 a
		_ -> error "never occur"

withPng' :: FilePath -> Margin -> CDouble -> Int -> (CairoT r RealWorld -> IO a) -> IO ()
withPng' fp m u n f = do
	s <- cairoImageSurfaceCreate CairoFormatArgb32 w h
	cr <- cairoCreate s
	_ <- f cr
	cairoImageSurfaceGetCairoImage s >>= \case
		CairoImageArgb32 a -> writePng fp $ cairoArgb32ToJuicyRGBA8 a
		_ -> error "never occur"
	where
	w = floor $ leftMargin m + u * fromIntegral n + rightMargin m
	h = floor $ topMargin m + u * fromIntegral n + bottomMargin m

grid :: PrimMonad m =>
	CairoT r (PrimState m) -> Margin -> CDouble -> Int -> m ()
grid cr m u n = do
	cairoSet cr $ LineWidth 1
	cairoSetSourceRgb cr . fromJust $ rgbDouble 0.6 0.6 0.6
	for_ [0 .. fromIntegral n] \y -> do
		cairoMoveTo cr 0 (topMargin m + u * y)
		cairoLineTo cr (leftMargin m + u * fromIntegral n + rightMargin m) (topMargin m + u * y)

	for_ [0 .. fromIntegral n] \x -> do
		cairoMoveTo cr (leftMargin m + u * x) 0
		cairoLineTo cr (leftMargin m + u * x) (topMargin m + u * fromIntegral n + bottomMargin m)

	cairoStroke cr

data Margin = Margin {
	topMargin :: CDouble, bottomMargin :: CDouble,
	leftMargin :: CDouble, rightMargin :: CDouble }
	deriving Show

strokeAndFill :: PrimMonad m => CairoT r (PrimState m) ->
	CDouble -> Rgb CDouble -> Rgb CDouble -> m ()
strokeAndFill cr lw cs cf = do
	cairoSet cr $ LineWidth lw
	cairoSetSourceRgb cr cs
	cairoStrokePreserve cr
	cairoSetSourceRgb cr cf
	cairoFill cr

coordinateX, coordinateY :: Margin -> CDouble -> CDouble -> CDouble
coordinateX m u x = leftMargin m + 3 * u * x
coordinateY m u y = topMargin m + 3 * u * y

fill :: PrimMonad m => CairoT r (PrimState m) -> Rgb CDouble -> m ()
fill cr c = do
	cairoSetSourceRgb cr c
	cairoFill cr

circle :: PrimMonad m => CairoT r (PrimState m) ->
	Margin -> CDouble -> CDouble -> CDouble -> CDouble -> m ()
circle cr m u x y r = do
	cairoMoveTo cr
		(coordinateX m u x + r) (coordinateY m u y)
	cairoArc cr
		(coordinateX m u x) (coordinateY m u y) r 0 (2 * pi)

rectangle :: PrimMonad m => CairoT r (PrimState m) ->
	Margin -> CDouble -> CDouble -> CDouble -> m ()
rectangle cr m u x y = cairoRectangle cr
	(coordinateX m u (x - 1 / 6)) (coordinateY m u (y - 1 / 6)) u u
