{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Foreign.C.Types
import Data.Maybe
import Data.Color
import System.Environment
import System.Console.GetOpt

import Graphics.Gdk.GdkDisplay
import Graphics.Gdk.GdkScreen
import Graphics.Gdk.Visuals
import Graphics.Gdk.Windows
import Graphics.Gdk.Windows.GdkWindowAttr
import Graphics.Gdk.Windows.GdkWindowAttr.Internal
import Graphics.Gdk.Windows.GdkEventMask
import Graphics.Gdk.EventStructures
import Graphics.Gdk.EventStructures.GdkKeySyms
import Graphics.Gdk.GdkDrawingContext

import Graphics.Cairo.Drawing.CairoT

import Try.Tools

main :: IO ()
main = do
	let	opts = [
			optTitle, optEvents, optPosition, optSize, optWclass,
			optVisual ]
	(ss, _as, es) <- getOpt Permute opts <$> getArgs
	dpy <- gdkDisplayOpen ""
	let	scr = gdkDisplayGetDefaultScreen dpy
	sysv <- gdkScreenGetSystemVisual scr
	rgbav <- gdkScreenGetRgbaVisual scr
	putStrLn `mapM_` es
	let	attr = optsToAttr sysv (fromMaybe sysv rgbav) ss
	print attr
	withGdkWindowAttr attr \x y -> print x >> print y
	w <- gdkWindowNew Nothing attr
	let	v = gdkWindowGetVisual w
	print sysv
	print rgbav
	print v
	print $ gdkVisualGetVisualType v
	print $ gdkVisualGetDepth v
	gdkWindowShow w
	mainLoop \case
		GdkEventGdkDelete _d -> pure False
		GdkEventGdkConfigure (gdkEventConfigure -> c) -> True <$ print c
		GdkEventGdkFocusChange (gdkEventFocus -> f) -> True <$ do
			print f
			r <- gdkWindowGetVisibleRegion w
			gdkWindowWithDrawFrame w r \dc -> do
				cr <- gdkDrawingContextGetCairoContext dc
				cairoSetSourceRgba cr . fromJust $ rgbaDouble 0 0.5 0 0.5
				cairoPaint cr
		GdkEventGdkKeyPress
			(gdkEventKeyKeyval . gdkEventKey -> GdkKey_q) ->
				pure False
		GdkEventGdkAny (gdkEventAny -> e) -> True <$ print e

data Visual = System | Rgba deriving (Show, Read, Eq)

data OptSetting
	= OptTitle String
	| OptEvents [GdkEventMaskSingleBit]
	| OptPosition (CInt, CInt)
	| OptSize (CInt, CInt)
	| OptWclass GdkWindowWindowClass
	| OptVisual Visual
	deriving Show

optTitle, optEvents, optPosition, optSize, optWclass, optVisual :: OptDescr OptSetting
optTitle = Option ['t'] ["title"] (ReqArg OptTitle "Title") "Set title"

optEvents = Option ['e'] ["events"] (ReqArg (OptEvents . read) "Event masks")
	"Set event mask"

optPosition = Option ['p'] ["position"] (ReqArg (OptPosition . read) "(X,Y)")
	"Set position"

optSize = Option ['s'] ["size"] (ReqArg (OptSize . read) "(W,H)")
	"Set size"

optWclass = Option ['w'] ["wclass"] (ReqArg (OptWclass . read) "Wclass")
	"Set wclass"

optVisual = Option ['v'] ["visual"] (ReqArg (OptVisual . read) "Visual")
	"Set visual"

optsToAttr :: GdkVisual -> GdkVisual -> [OptSetting] -> GdkWindowAttr
optsToAttr _sysv _rgbav [] = minimalGdkWindowAttr
	(gdkEventMaskMultiBits [GdkKeyPressMask, GdkFocusChangeMask])
	100 100 GdkInputOutput GdkWindowToplevel
optsToAttr sysv rgbav (OptTitle t : ss) = (optsToAttr sysv rgbav ss) { gdkWindowAttrTitle = Just t }
optsToAttr sysv rgbav (OptEvents es : ss) =
	(optsToAttr sysv rgbav ss) { gdkWindowAttrEventMask = gdkEventMaskMultiBits es }
optsToAttr sysv rgbav (OptPosition (x, y) : ss) =
	(optsToAttr sysv rgbav ss) { gdkWindowAttrX = Just x, gdkWindowAttrY = Just y }
optsToAttr sysv rgbav (OptSize (w, h) : ss) =
	(optsToAttr sysv rgbav ss) { gdkWindowAttrWidth = w, gdkWindowAttrHeight = h }
optsToAttr sysv rgbav (OptWclass w : ss) =
	(optsToAttr sysv rgbav ss) { gdkWindowAttrWclass = w }
optsToAttr sysv rgbav (OptVisual System : ss) =
	(optsToAttr sysv rgbav ss) { gdkWindowAttrVisual = Just sysv }
optsToAttr sysv rgbav (OptVisual Rgba : ss) =
	(optsToAttr sysv rgbav ss) { gdkWindowAttrVisual = Just rgbav }
