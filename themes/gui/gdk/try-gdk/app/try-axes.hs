{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Control.Monad
import Data.Foldable
import Data.Maybe
import Data.KeySym
import System.Environment
import System.Console.GetOpt
import Graphics.Gdk.GdkDisplay
import Graphics.Gdk.GdkSeat
import Graphics.Gdk.GdkDevice
import Graphics.Gdk.GdkDevice.GdkAxes
import Graphics.Gdk.PropertiesAndAtoms.GdkAtom
import Graphics.Gdk.Windows
import Graphics.Gdk.Windows.GdkWindowAttr
import Graphics.Gdk.Windows.GdkEventMask
import Graphics.Gdk.EventStructures

import Try.Tools

main :: IO ()
main = do
	(ss, _as, es) <- getOpt Permute [
		optPointer, optKeyboard, optMotion, optSetAxisUse ] <$> getArgs
	putStrLn `mapM_` es

	dpy <- gdkDisplayOpen ""
	st <- gdkDisplayGetDefaultSeat dpy
	pnt <- gdkSeatGetPointer st

	when (OptSetAxisUse `elem` ss) do
		gdkDeviceSetAxisUse pnt 0 GdkAxisX
		gdkDeviceSetAxisUse pnt 1 GdkAxisY
		gdkDeviceSetAxisUse pnt 2 GdkAxisPressure

	when (OptPointer `elem` ss) do
		pnts <- gdkDeviceListSlaveDevices pnt
		printAxis pnt
		printAxis `mapM_` pnts
	
	when (OptKeyboard `elem` ss) do
		kbd <- gdkSeatGetKeyboard st
		kbds <- gdkDeviceListSlaveDevices kbd
		printKeys kbd
		printKeys `mapM_` kbds

	when (OptMotion `elem` ss) do
		w <- gdkToplevelNew Nothing
			$ minimalGdkWindowAttr
				(gdkEventMaskMultiBits eventMask) 100 100
		gdkWindowSetEventCompression w False
		gdkWindowShow w
		gdkDisplayFlush dpy
		mainLoop \case
			GdkEventGdkDelete _d -> pure False
			GdkEventGdkKeyPress e -> do
				k <- gdkEventKey e
				print k
				pure case gdkEventKeyKeyval k of
					Xk_q -> False; _ -> True
			GdkEventGdkMotionNotify e -> True <$ do
				m <- gdkEventMotion e
				printAxisValues (gdkEventMotionDevice m) (gdkEventMotionAxes m)
				printAxisValues (fromJust $ gdkEventMotionSourceDevice m) (gdkEventMotionAxes m)
				printAxisValuesFromStr (gdkEventMotionDevice m) (gdkEventMotionAxes m)
			GdkEventGdkAny e -> True <$ (print =<< gdkEventAny e)

printAxis :: IsGdkDevice d => d 'Pointer -> IO ()
printAxis d = do
	putStrLn =<< gdkDeviceGetName d
	putStrLn . ("\tgdkDeviceGetAxes: " ++) . show . gdkAxisFlagList
		=<< gdkDeviceGetAxes d
	n <- gdkDeviceGetNAxes d
	for_ [0 .. fromIntegral $ n - 1] \i ->
		putStrLn . ("\tgdkDeviceGetAxisUse " ++) . (show i ++)
			. (": " ++) . show =<< gdkDeviceGetAxisUse d i
	putStrLn . ("\tgdkDeviceListAxes: " ++) . show
		=<< mapM gdkAtomName =<< gdkDeviceListAxes d
	putStrLn ""

printKeys :: IsGdkDevice d => d 'Keyboard -> IO ()
printKeys d = do
	putStrLn =<< gdkDeviceGetName d
	putStrLn . ("\tgdkDeviceGetNKeys: " ++) . show =<< gdkDeviceGetNKeys d
	putStrLn ""

data OptSetting
	= OptPointer
	| OptKeyboard
	| OptMotion
	| OptSetAxisUse
	deriving (Show, Eq)

optPointer, optKeyboard, optMotion, optSetAxisUse :: OptDescr OptSetting
optPointer = Option ['p'] ["pointer"] (NoArg OptPointer) "Show pointers"
optKeyboard = Option ['k'] ["keyboard"] (NoArg OptKeyboard) "Show keyboards"
optMotion = Option ['m'] ["motion"] (NoArg OptMotion) "Show motion"
optSetAxisUse = Option ['u'] ["set-axis-use"] (NoArg OptSetAxisUse)
	"Set axis use"

eventMask :: [GdkEventMaskSingleBit]
eventMask = [
	GdkKeyPressMask, GdkPointerMotionMask
	]

printAxisValues :: IsGdkDevice d => d 'Pointer -> GdkAxes -> IO ()
printAxisValues d as = do
	putStrLn =<< gdkDeviceGetName d
	for_ axisNameUsePairs \(n, u) ->
		putStrLn . ("GdkAxis" ++) . (n ++) . (": " ++) . show
			=<< gdkDeviceGetAxis d as u

axisNameUsePairs :: [(String, GdkAxisUse)]
axisNameUsePairs = [
	("X", GdkAxisX), ("Y", GdkAxisY), ("Pressure", GdkAxisPressure),
	("Xtilt", GdkAxisXtilt), ("Ytilt", GdkAxisYtilt),
	("Wheel", GdkAxisWheel), ("Distance", GdkAxisDistance),
	("Rotation", GdkAxisRotation), ("Slider", GdkAxisSlider) ]

printAxisValuesFromStr :: IsGdkDevice d => d 'Pointer -> GdkAxes -> IO ()
printAxisValuesFromStr d as = do
	printAxisValueFromStr d as `mapM_` [
		"Rel X", "Rel Y", "Rel Horiz Wheel", "Rel Vert Wheel",
		"Abs X", "Abs Y", "Abs Pressure", "Abs Tilt X", "Abs Tilt Y",
		"Abs Wheel" ]

printAxisValueFromStr :: IsGdkDevice d => d 'Pointer -> GdkAxes -> String -> IO ()
printAxisValueFromStr d as str = do
	mv <- gdkDeviceGetAxisValue d as =<< gdkAtomIntern str
	putStrLn $ str ++ ": " ++ show mv
