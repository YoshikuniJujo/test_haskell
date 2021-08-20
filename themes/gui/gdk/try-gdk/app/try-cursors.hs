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
		optSurface, optName, optCursorType ] <$> getArgs
	putStrLn `mapM_` es
	dpy <- gdkDisplayOpen ""
	print dpy
	c0 <- gdkCursorNewForDisplay dpy GdkArrow
	print $ gdkCursorGetDisplay c0
	w <- gdkToplevelNew Nothing $ minimalGdkWindowAttr
		(gdkEventMaskMultiBits []) 700 450 GdkInputOutput GdkWindowToplevel
	gdkWindowShow w
	c <- optionsToCursor dpy os
	gdkWindowSetCursor w c
	print =<< gdkCursorGetCursorType c

	doWhile_ $ gdkWithEventGet $ pure . maybe False (const True)

	gdkDisplayFlush dpy
	void getLine

drawCursor :: PrimMonad m => CairoFormatT -> m (CairoSurfaceImageT s (PrimState m))
drawCursor cf = do
	s <- cairoImageSurfaceCreate cf 50 50
	cr <- cairoCreate s
	cairoSetSourceRgba cr . fromJust $ rgbaDouble 0 1 0 0.5
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
	| OptName String
	| OptCursorType GdkCursorType
	deriving Show

data Format = Argb32 | Rgb24 | A8 | A1 | Rgb16565 | Rgb30 deriving Show

optSurface, optName, optCursorType :: OptDescr Option
optSurface = Option ['s'] ["surface"]
	(ReqArg (OptSurface . format) "Image Format") "From Surface"

optName = Option ['n'] ["name"] (ReqArg OptName "Name") "From Name"

optCursorType = Option ['t'] ["cursor-type"]
	(ReqArg (OptCursorType . cursorTypeFromStr) "Cursor Type")
	"From Cursor Type"

format :: String -> Format
format "Rgb24" = Rgb24
format "A8" = A8
format "A1" = A1
format "Rgb16565" = Rgb16565
format "Rgb30" = Rgb30
format _ = Argb32

fromFormat :: Format -> CairoFormatT
fromFormat Argb32 = cairoFormatArgb32
fromFormat Rgb24 = cairoFormatRgb24
fromFormat A8 = cairoFormatA8
fromFormat A1 = cairoFormatA1
fromFormat Rgb16565 = cairoFormatRgb16565
fromFormat Rgb30 = cairoFormatRgb30

cursorTypeFromStr :: String -> GdkCursorType
cursorTypeFromStr "XCursor" = GdkXCursor
cursorTypeFromStr "Arrow" = GdkArrow
cursorTypeFromStr "BasedArrowDown" = GdkBasedArrowDown
cursorTypeFromStr "BasedArrowUp" = GdkBasedArrowUp
cursorTypeFromStr "Gumby" = GdkGumby
cursorTypeFromStr "Boat" = GdkBoat
cursorTypeFromStr "Watch" = GdkWatch
cursorTypeFromStr "Xterm" = GdkXterm
cursorTypeFromStr "BlankCursor" = GdkBlankCursor
cursorTypeFromStr _ = GdkXCursor

optionsToCursor :: GdkDisplay -> [Option] -> IO GdkCursor
optionsToCursor dpy [] = do
	s <- drawCursor cairoFormatArgb32
	gdkCursorNewFromSurface dpy s 15 15
optionsToCursor dpy (OptSurface f : _) = do
	s <- drawCursor $ fromFormat f
	gdkCursorNewFromSurface dpy s 15 15
optionsToCursor dpy (OptName n : _) = gdkCursorNewFromName dpy n
optionsToCursor dpy (OptCursorType t : _) = gdkCursorNewForDisplay dpy t
