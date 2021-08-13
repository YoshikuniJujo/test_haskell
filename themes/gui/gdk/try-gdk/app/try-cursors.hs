{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Control.Monad
import Control.Monad.Primitive
import Data.Maybe
import Data.Color
import System.Environment
import System.Console.GetOpt

import Graphics.Gdk.GdkDisplay
import Graphics.Gdk.Cursors
import Graphics.Gdk.Windows
import Graphics.Gdk.Windows.GdkWindowAttr
import Graphics.Gdk.Windows.GdkEventMask
import Graphics.Gdk.Events

import Graphics.Cairo.Drawing.CairoT
import Graphics.Cairo.Drawing.Paths
import Graphics.Cairo.Surfaces.ImageSurfaces
import Graphics.Cairo.Values

import Try.Tools.DoWhile

main :: IO ()
main = do
	(os, _as, es) <- getOpt Permute [
		optSurface ] <$> getArgs
	putStrLn `mapM_` es
	dpy <- gdkDisplayOpen ""
	print dpy
	c0 <- gdkCursorNewForDisplay dpy GdkArrow
	print $ gdkCursorGetDisplay c0
	w <- gdkWindowNew Nothing $ minimalGdkWindowAttr
		(gdkEventMaskMultiBits []) 500 350 GdkInputOutput GdkWindowToplevel
	gdkWindowShow w
	gdkWindowSetCursor w =<< optionsToCursor dpy os

	doWhile_ $ gdkWithEventGet $ pure . maybe False (const True)

	gdkDisplayFlush dpy
	void getLine

drawCursor :: PrimMonad m => CairoFormatT -> m (CairoSurfaceImageT s (PrimState m))
drawCursor cf = do
	s <- cairoImageSurfaceCreate cf 50 50
	cr <- cairoCreate s
	cairoSetSourceRgb cr . fromJust $ rgbDouble 0 1 0
	cairoSetLineWidth cr 3
	cairoMoveTo cr 15 15
	cairoLineTo cr 15 30
	cairoStroke cr
	cairoMoveTo cr 15 15
	cairoLineTo cr 30 15
	cairoStroke cr
	cairoMoveTo cr 15 15
	cairoLineTo cr 35 35
	cairoStroke cr
	pure s

data Option
	= OptSurface Format
	deriving Show

data Format = Argb32 | Rgb24 deriving Show

optSurface :: OptDescr Option
optSurface = Option ['s'] ["surface"]
	(ReqArg (OptSurface . format) "Image Format") "From Surface"

format :: String -> Format
format "Rgb24" = Rgb24
format _ = Argb32

fromFormat :: Format -> CairoFormatT
fromFormat Argb32 = cairoFormatArgb32
fromFormat Rgb24 = cairoFormatRgb24

optionsToCursor :: GdkDisplay -> [Option] -> IO GdkCursor
optionsToCursor dpy [] = do
	s <- drawCursor cairoFormatArgb32
	gdkCursorNewFromSurface dpy s 15 15
optionsToCursor dpy (OptSurface f : _) = do
	s <- drawCursor $ fromFormat f
	gdkCursorNewFromSurface dpy s 15 15
