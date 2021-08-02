{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Control.Monad
import Control.Exception
import System.Environment
import System.Console.GetOpt

import Graphics.Gdk.General
import Graphics.Gdk.GdkDisplay
import Graphics.Gdk.GdkSeat
import Graphics.Gdk.Windows
import Graphics.Gdk.EventStructures
import Graphics.Gdk.EventStructures.GdkKeySyms
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
	st <- gdkDisplayGetDefaultSeat dd
	ptr <- gdkSeatGetPointer st
	print ptr
	print =<< gdkDisplayDeviceIsGrabbed dd ptr
	case settingsMyDisplayName sts of
		Just n -> do
			nd <- gdkDisplayOpen n
			print $ gdkDisplayGetName nd
			gdkDisplayClose nd
		Nothing -> pure ()
	w <- gdkWindowNew Nothing defaultGdkWindowAttr
	gdkWindowShow w
	mainLoopDisplay dd \case
		GdkEventGdkDelete _d -> pure False
		GdkEventGdkKeyPress k_ -> do
			case gdkEventKey k_ of
				GdkEventKey { gdkEventKeyKeyval = GdkKey_q } ->
					pure False
				GdkEventKey { gdkEventKeyKeyval = GdkKey_g } ->
					True <$ gdkSeatGrabSimple st w
				GdkEventKey { gdkEventKeyKeyval = GdkKey_u } ->
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
