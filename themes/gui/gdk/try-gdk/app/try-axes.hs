{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Control.Monad
import System.Environment
import System.Console.GetOpt
import Graphics.Gdk.GdkDisplay
import Graphics.Gdk.GdkSeat
import Graphics.Gdk.GdkDevice
import Graphics.Gdk.GdkDevice.GdkAxes

main :: IO ()
main = do
	(ss, _as, es) <- getOpt Permute [
		optPointer, optKeyboard ] <$> getArgs
	putStrLn `mapM_` es

	dpy <- gdkDisplayOpen ""
	st <- gdkDisplayGetDefaultSeat dpy

	when (OptPointer `elem` ss) do
		pnt <- gdkSeatGetPointer st
		pnts <- gdkDeviceListSlaveDevices pnt
		printAxis pnt
		printAxis `mapM_` pnts
	
	when (OptKeyboard `elem` ss) do
		kbd <- gdkSeatGetKeyboard st
		kbds <- gdkDeviceListSlaveDevices kbd
		printKeys kbd
		printKeys `mapM_` kbds

printAxis :: IsGdkDevice d => d 'Pointer -> IO ()
printAxis d = do
	putStrLn =<< gdkDeviceGetName d
	putStrLn . ("\tgdkDeviceGetNAxes: " ++) . show =<< gdkDeviceGetNAxes d
	putStrLn . ("\tgdkDeviceGetAxes: " ++) . show . gdkAxisFlagList
		=<< gdkDeviceGetAxes d
	putStrLn ""

printKeys :: IsGdkDevice d => d 'Keyboard -> IO ()
printKeys d = do
	putStrLn =<< gdkDeviceGetName d
	putStrLn . ("\tgdkDeviceGetNKeys: " ++) . show =<< gdkDeviceGetNKeys d
	putStrLn ""

data OptSetting
	= OptPointer
	| OptKeyboard
	deriving (Show, Eq)

optPointer, optKeyboard :: OptDescr OptSetting
optPointer = Option ['p'] ["pointer"] (NoArg OptPointer) "Show pointers"
optKeyboard = Option ['k'] ["keyboard"] (NoArg OptKeyboard) "Show keyboards"
