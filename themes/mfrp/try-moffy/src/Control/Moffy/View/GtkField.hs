{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DataKinds, TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs -fno-warn-orphans #-}

module Control.Moffy.View.GtkField (
	drawText, drawLine, drawImage, drawBox, fillPolygon ) where

import qualified Data.ByteString as BS

import Control.Concurrent.STM

import Graphics.Gtk
import Graphics.Cairo
import Graphics.Pango
import Graphics.CairoType

import Control.Moffy.Viewable.Basic
import Control.Moffy.Viewable.Text
import Control.Moffy.Viewable.Shape
import Control.Moffy.Viewable.Image

---------------------------------------------------------------------------

-- TEXT

drawText :: GtkWidget -> CairoT -> VText -> IO ()
drawText _ cr (Text' c fn fs (x, y) t) = do
	l <- pangoCairoCreateLayout cr
	d <- pangoFontDescriptionNew
	pangoFontDescriptionSetFamily d fn
	pangoFontDescriptionSetAbsoluteSize d fs
	pangoLayoutSetFontDescription l d
	pangoLayoutSetText l t
	uncurry3 (cairoSetSourceRgb cr) $ colorToRgb c
	cairoMoveTo cr x y
	pangoCairoShowLayout cr l

-- LINE

drawLine :: a -> CairoT -> Line -> IO ()
drawLine _ cr (Line' c w (xb, yb) (xe, ye)) = do
	uncurry3 (cairoSetSourceRgb cr) $ colorToRgb c
	cairoSetLineWidth cr w
	cairoMoveTo cr xb yb
	cairoLineTo cr xe ye
	cairoStroke cr

-- IMAGE

drawImage :: GtkWidget -> CairoT -> Image -> IO ()
drawImage _ cr (Image' (x, y) (Png w h bs)) = do
	tbs <- atomically $ newTVar bs
	cairoWithImageSurfaceFromPngStream (bsToCairoReadFunc tbs) () \png -> do
		w0 <- cairoImageSurfaceGetWidth png
		h0 <- cairoImageSurfaceGetHeight png
		cairoScale cr (w / fromIntegral w0) (h / fromIntegral h0)
		cairoSetSourceSurface cr png (x * fromIntegral w0 / w) (y * fromIntegral h0 / h)
		cairoPaint cr
		cairoIdentityMatrix cr

bsToCairoReadFunc :: TVar BS.ByteString -> CairoReadFunc ()
bsToCairoReadFunc tbs () (fromIntegral -> n) = atomically $ readTVar tbs >>= \bs -> case () of
	_	| BS.length bs >= n -> (cairoStatusSuccess, Just t) <$ writeTVar tbs d
		| otherwise -> pure (cairoStatusReadError, Nothing)
		where (t, d) = BS.splitAt n bs

drawBox :: a -> CairoT -> Box -> IO ()
drawBox _ cr (Box (Rect (l_, u_) (r, d)) c) = do
	uncurry3 (cairoSetSourceRgb cr) $ bcolorToRgb c
	cairoRectangle cr l u w h
	cairoStrokePreserve cr
	cairoFill cr
	where
	l = min l_ r
	u = min u_ d
	w = abs $ l_ - r
	h = abs $ u_ - d

fillPolygon :: a -> CairoT -> FillPolygon -> IO ()
fillPolygon _ _ (FillPolygon _ []) = pure ()
fillPolygon _ cr (FillPolygon c (p : ps)) = do
	uncurry3 (cairoSetSourceRgb cr) $ colorToRgb c
	uncurry (cairoMoveTo cr) p
	uncurry (cairoLineTo cr) `mapM_` ps
	cairoFill cr

uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (x, y, z) = f x y z

bcolorToRgb :: BColor -> (Double, Double, Double)
bcolorToRgb Red = (1, 0, 0)
bcolorToRgb Green = (0, 1, 0)
bcolorToRgb Blue = (0, 0, 1)
bcolorToRgb Yellow = (1, 1, 0)
bcolorToRgb Cyan = (0, 1, 1)
bcolorToRgb Magenta = (1, 0, 1)
