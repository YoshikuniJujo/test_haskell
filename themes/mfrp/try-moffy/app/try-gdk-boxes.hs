{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Control.Moffy
import Control.Moffy.Viewable.Shape
import Data.Maybe
import Data.Color
import System.Environment
import System.Console.GetOpt
import Graphics.Cairo.Drawing.CairoT
import Graphics.Gdk.Windows
import Graphics.Gdk.GdkDrawingContext

import Trial.TryGdk
import Trial.Boxes

main :: IO ()
main = do
	(os, _as, ems) <- getOpt Permute [optColor] <$> getArgs
	putStrLn `mapM_` ems
	case os of
		[OptColor] -> tryGdk showColor $ adjustSig cycleColor
		_ -> putStrLn "Bad options"

data OptSetting
	= OptColor
	deriving (Show, Eq)

optColor :: OptDescr OptSetting
optColor = Option ['c'] ["color"] (NoArg OptColor) "Cycle color"

showColor :: GdkWindow -> BColor -> IO ()
showColor w bc = do
	r <- gdkWindowGetVisibleRegion w
	gdkWindowWithDrawFrame w r \ctx -> do
		cr <- gdkDrawingContextGetCairoContext ctx
		cairoSetSourceRgb cr $ bColorToRgb bc
		cairoPaint cr

bColorToRgb :: BColor -> Rgb
bColorToRgb Red = fromJust $ rgbDouble 0.5 0 0
bColorToRgb Green = fromJust $ rgbDouble 0 0.5 0
bColorToRgb Blue = fromJust $ rgbDouble 0 0 0.5
bColorToRgb Yellow = fromJust $ rgbDouble 0.5 0.5 0
bColorToRgb Cyan = fromJust $ rgbDouble 0 0.5 0.5
bColorToRgb Magenta = fromJust $ rgbDouble 0.5 0 0.5
