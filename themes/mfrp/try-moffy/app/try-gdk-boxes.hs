{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Prelude hiding (repeat)

import Control.Moffy
import Control.Moffy.Event.Mouse.DefaultWindow
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
import Trial.Paper

main :: IO ()
main = do
	(os, _as, ems) <- getOpt Permute [
		optColor, optMousePos, optCurRect, optWiggleRect, optPosInside,
		optFirstPoint ] <$> getArgs
	putStrLn `mapM_` ems
	case os of
		[OptColor] -> tryGdk showColor $ adjustSig cycleColor
		[OptMousePos] -> tryGdk @_ @() (const print) $ adjustSig mousePos
		[OptCurRect] -> tryGdk @_ @() showRect . adjustSig $ curRect (100, 100)
		[OptWiggleRect] -> tryGdk @_ @() showRect
			. adjustSig . wiggleRect $ Rect (300, 200) (600, 400)
		[OptPosInside] -> tryGdk @_ @() (const print) . adjustSig
			. repeat $ posInside
				(Rect (300, 200) (600, 400)) mousePos
		[OptFirstPoint] -> tryGdk @_ @() (const print) . adjustSig
			$ repeat firstPoint
		_ -> putStrLn "Bad options"

data OptSetting
	= OptColor | OptMousePos | OptCurRect | OptWiggleRect | OptPosInside
	| OptFirstPoint
	deriving (Show, Eq)

optColor, optMousePos, optCurRect, optWiggleRect, optPosInside, optFirstPoint
	:: OptDescr OptSetting
optColor = Option ['c'] ["color"] (NoArg OptColor) "Cycle color"
optMousePos = Option ['m'] ["mouse-pos"] (NoArg OptMousePos) "Mouse position"
optCurRect = Option ['r'] ["cur-rect"] (NoArg OptCurRect) "Cur rect"
optWiggleRect = Option ['w'] ["wiggle-rect"] (NoArg OptWiggleRect) "Wiggle rect"
optPosInside = Option ['p'] ["pos-inside"] (NoArg OptPosInside) "Pos inside"
optFirstPoint = Option ['f'] ["first-point"] (NoArg OptFirstPoint) "First point"

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
