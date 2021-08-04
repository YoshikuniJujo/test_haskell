{-# LANGUAGE BlockArguments, OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Foreign.C.Types
import Control.Monad
import Data.Maybe
import Data.List
import Data.Word
import Data.Int
import Data.Color
import System.Environment

import Graphics.Gdk.GdkDisplay
import Graphics.Gdk.GdkScreen
import Graphics.Gdk.Visuals
import Graphics.Gdk.Windows
import Graphics.Gdk.GdkDrawingContext

import Graphics.Cairo.Drawing.CairoT
import Graphics.Cairo.Drawing.Regions
import Graphics.Cairo.Drawing.Paths
import Graphics.Cairo.Utilities.Types

import Graphics.Pango.Basic.Fonts.PangoFontDescription
import Graphics.Pango.Basic.LayoutObjects.PangoLayout
import Graphics.Pango.Rendering.Cairo

import Try.Tools

import qualified Data.Text as T

main :: IO ()
main = do
	as <- getArgs
	let	rsl = case as of [] -> 24; r_ : _ -> read r_
	void $ gdkDisplayOpen ""
	mscr <- gdkScreenGetDefault
	print mscr
	maybe (putStrLn "No default screens") (run rsl) mscr
	win <- gdkWindowNew Nothing defaultGdkWindowAttr
	gdkWindowShow win
	gdkDisplayFlush =<< gdkDisplayGetDefault
	void getLine
	r <- cairoRegionCreateRectangle $ CairoRectangleIntT 50 50 200 200
	gdkWindowWithDrawFrame win r \cxt -> do
		cr <- gdkDrawingContextGetCairoContext cxt
		cairoSetSourceRgb cr . fromJust $ rgbDouble 0.8 0.2 0.2
		cairoRectangle cr 10 10 100 100
		cairoFill cr
		cairoMoveTo cr 50 50
		cairoSetSourceRgb cr . fromJust $ rgbDouble 0.8 0.8 0.8
		ctx <- pangoCairoCreateContext cr
		print =<< pangoCairoContextGetResolution ctx
		pangoCairoContextSetResolution ctx $ Just rsl
		pl <- pangoLayoutNew ctx
		fd <- pangoFontDescriptionNew
		pangoFontDescriptionSet fd $ Size 24
		pangoLayoutSet pl . pangoFontDescriptionToNullable . Just =<< pangoFontDescriptionFreeze fd
		pangoLayoutSet @T.Text pl "Hello, world!"
		pangoCairoShowLayout cr =<< pangoLayoutFreeze pl
	gdkDisplayFlush =<< gdkDisplayGetDefault
	void getLine
	gdkWindowDestroy win
	gdkDisplayClose =<< gdkDisplayGetDefault

run :: CDouble -> GdkScreen -> IO ()
run r scr = do
	printVisual "System Visual" =<< gdkScreenGetSystemVisual scr
	maybe (putStrLn "No rgba visuals") (printVisual "Rgba Visual")
		=<< gdkScreenGetRgbaVisual scr
	mapM_ print . map packGroup . group =<< mapM peekVisual =<< gdkScreenListVisuals scr
	putStr "gdkScreenIsComposited: "
	print =<< gdkScreenIsComposited scr
	print =<< gdkWindowGetWindowType =<< gdkScreenGetRootWindow scr
	print =<< mapM gdkWindowGetWindowType =<< gdkScreenGetToplevelWindows scr
	maybe (putStrLn "No Window Stack") (mapM_ printWindowStack) =<< gdkScreenGetWindowStack scr
	print =<< gdkDisplayGetDefault
	print =<< gdkScreenGetDisplay scr
	print =<< gdkScreenGetResolution scr
	gdkScreenSetResolution scr r
	print =<< gdkScreenGetResolution scr

printWindowStack :: GdkWindowAutoUnref -> IO ()
printWindowStack wau = withGdkWindowAutoUnref wau \w ->
	print =<< gdkWindowGetOrigin w

printVisual :: String -> GdkVisual -> IO ()
printVisual ttl v = do
	t <- gdkVisualGetVisualType v
	d <- gdkVisualGetDepth v
	r <- gdkVisualGetRedPixelDetails v
	g <- gdkVisualGetGreenPixelDetails v
	b <- gdkVisualGetBluePixelDetails v
	putStrLn ttl
	putStrLn $ "\tVisualType       : " ++ show t
	putStrLn $ "\tDepth            : " ++ show d
	putStrLn $ "\tRedPixelDetails  : " ++ show r
	putStrLn $ "\tGreenPixelDetails: " ++ show g
	putStrLn $ "\tBluePixelDetails : " ++ show b

type PixelDetails = (Word32, Int32, Int32)

peekVisual :: GdkVisual ->
	IO (GdkVisualType, Int32, PixelDetails, PixelDetails, PixelDetails)
peekVisual v = do
	t <- gdkVisualGetVisualType v
	d <- gdkVisualGetDepth v
	r <- gdkVisualGetRedPixelDetails v
	g <- gdkVisualGetGreenPixelDetails v
	b <- gdkVisualGetBluePixelDetails v
	pure (t, d, r, g, b)

packGroup :: [a] -> (Int, a)
packGroup [] = error "bad"
packGroup xa@(x : _) = (length xa, x)
