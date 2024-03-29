{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Control.Monad
import Control.Exception
import Data.KeySym
import System.Environment
import System.Console.GetOpt

import Graphics.Gdk.General
import Graphics.Gdk.GdkDisplay
import Graphics.Gdk.GdkScreen
import Graphics.Gdk.GdkSeat
import Graphics.Gdk.GdkMonitor
import Graphics.Gdk.Windows
import Graphics.Gdk.EventStructures
import Graphics.Gdk.Exception

import Try.Tools

main :: IO ()
main = do
	(print =<< gdkDisplayGetDefault) `catch` \(e :: GdkNoDefaultDisplay) -> print e
	(_pn, args) <- join $ gdkInit <$> getProgName <*> getArgs
	let	(os, as, es) = getOpt Permute optDescrs args
		sts = optionsToSettings os
	print sts
	print as
	print es
	dd <- gdkDisplayGetDefault
	print $ gdkDisplayGetName dd
	let	scr = gdkDisplayGetDefaultScreen dd
	print scr
	wr <- gdkScreenGetRootWindow scr
	print wr
	st <- gdkDisplayGetDefaultSeat dd
	print st
	print =<< gdkDisplayListSeats dd
	print =<< gdkDisplayGetDefaultGroup dd
	ptr <- gdkSeatGetPointer st
	print ptr
	print =<< gdkDisplayDeviceIsGrabbed dd ptr

	putStr "gdkDisplayGetNMonitors: "
	print =<< gdkDisplayGetNMonitors dd
	printMonitor =<< gdkDisplayGetMonitor dd 0
	maybe (putStrLn "no primary monitors") printMonitor =<< gdkDisplayGetPrimaryMonitor dd
	printMonitor =<< gdkDisplayGetMonitorAtPoint dd 100 100
	printMonitor =<< gdkDisplayGetMonitorAtWindow dd wr

	case settingsMyDisplayName sts of
		Just n -> do
			nd <- gdkDisplayOpen n
			print $ gdkDisplayGetName nd
			gdkDisplayClose nd
		Nothing -> pure ()
	w <- gdkToplevelNew Nothing defaultGdkWindowAttr
	gdkWindowShow w
	putStr "gdkDisplaySupportsCursorColor: "
	print =<< gdkDisplaySupportsCursorColor dd
	putStr "gdkDisplaySupportCursorAlpha: "
	print =<< gdkDisplaySupportsCursorAlpha dd
	putStr "gdkDisplayGetDefaultCursorSize: "
	print =<< gdkDisplayGetDefaultCursorSize dd
	mainLoopDisplay dd \case
		GdkEventGdkDelete _d -> pure False
		GdkEventGdkKeyPress k_ -> gdkEventKey k_ >>= \case
			GdkEventKey { gdkEventKeyKeyval = Xk_q } ->
				pure False
			GdkEventKey { gdkEventKeyKeyval = Xk_g } ->
				True <$ gdkSeatGrabSimple st w
			GdkEventKey { gdkEventKeyKeyval = Xk_u } ->
				True <$ gdkSeatUngrab st
			k -> True <$ print k
		e -> True <$ do
			print e
			print =<< gdkDisplayDeviceIsGrabbed dd ptr

data Settings = Settings {
	settingsMyDisplayName :: Maybe String
	} deriving Show

initialSettings :: Settings
initialSettings = Settings {
	settingsMyDisplayName = Nothing
	}

data Option
	= MyDisplayName String
	deriving Show

optDescrs :: [OptDescr Option]
optDescrs = [
	Option "" ["my-display"] (ReqArg MyDisplayName "My display name")
		"Set my display name"
	]

optionSet :: Option -> Settings -> Settings
optionSet (MyDisplayName dn) s = s { settingsMyDisplayName = Just dn }

optionsToSettings :: [Option] -> Settings
optionsToSettings [] = initialSettings
optionsToSettings (o : os) = optionSet o $ optionsToSettings os

printMonitor :: GdkMonitor -> IO ()
printMonitor m = do
	mn <- gdkMonitorGetManufacturer m
	md <- gdkMonitorGetModel m
	putStrLn "Monitor: "
	putStrLn $ "\tManufacturer: " ++ show mn
	putStrLn $ "\tModel       : " ++ show md
