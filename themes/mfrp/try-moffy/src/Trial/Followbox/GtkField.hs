{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DataKinds, TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs -fno-warn-orphans #-}

module Trial.Followbox.GtkField (
	-- * VIEW
	View(..), View1,
	-- * COLOR
	Color(..), white, blue, Png(..),
	-- * TEMP
	VText(..), Line(..), Image(..)
	) where

import qualified Data.ByteString as BS
import qualified Data.Text as T

import Control.Moffy.Handle.GtkField
import Control.Concurrent.STM

import Graphics.Gtk
import Graphics.Gtk.Cairo
import Graphics.Gtk.Cairo.Values
import Graphics.Gtk.Pango

import Data.OneOfThem

import Trial.Followbox.ViewType

import Trial.Followbox.Basic

---------------------------------------------------------------------------

-- TEXT

drawText :: GtkWidget -> CairoT -> VText -> IO ()
drawText _ cr (Text' c fn fs (x, y) t) = do
	l <- pangoCairoCreateLayout cr
	d <- pangoFontDescriptionFromString $ T.pack fn
	pangoFontDescriptionSetAbsoluteSize d fs
	pangoLayoutSetFontDescription l d
	pangoLayoutSetText l t
	uncurry3 (cairoSetSourceRgb cr) $ colorToRgb c
	cairoMoveTo cr x y
	pangoCairoShowLayout cr l

uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (x, y, z) = f x y z

-- LINE

drawLine :: GtkWidget -> CairoT -> Line -> IO ()
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

-- VIEW

instance Drawable View where
	draw wdt cr (View v) = do
		w <- gtkWidgetGetAllocatedWidth wdt
		h <- gtkWidgetGetAllocatedHeight wdt
		print (w, h)
		cairoSetSourceRgb cr 0 0 0
		cairoRectangle cr 0 0 (fromIntegral w) (fromIntegral h)
		cairoStrokePreserve cr
		cairoFill cr
		((drawText wdt cr >-- drawLine wdt cr >-- SingletonFun (drawImage wdt cr)) `apply`) `mapM_` v
